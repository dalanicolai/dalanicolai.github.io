---
title: Fixing org-mode coding assistance 3
date: 2023-09-06 15:48:45 +0200
categories: [Emacs]
tags: [emacs, orgbabel]
---


## In org static coding assistance with anaconda-mode

Here we implement static coding assistance in an org buffer for a
single virtual environment (and 'assuming' that the org buffer is the
only file in the project). As the LSP protocol is difficult to hack
on, we simply use `anaconda-mode` (which is equivalent to using [pylsp](https://github.com/python-lsp/python-lsp-server)
as they both rely [jedi](https://jedi.readthedocs.io/en/latest/) for completion, signature hints and goto
definition).  We use a simple trick to make anaconda-mode work well
with org buffers; we simply comment out the non-python lines before
sending the buffer contents to the jedi server (as it works even
better, we actually replace non-python lines by a line of line length
79 of only `#` characters). As buffer positions are expressed in line
an column numbers (i.e. not by `point`), the extra buffer contents
does not affect the jedi/flymake functionality. We keep it simple, and
assume that all python blocks are part of the code (you can simply
modify/extend the 'filter' if that is not the case for you):

{% highlight emacs-lisp %}
(defun org-babel-python-filter (contents)
  "Comment out lines outside of python src-blocks.
The filter does not differentiate code blocks with different
session/kernel/tangle values."
  (let ((lines (split-string contents "\n"))
	(replace (make-string 79 (string-to-char "#")))
        in)
    (mapconcat #'identity
               (mapcar (lambda (line)
                         (let (l)
                           (when (string-match-p "^#\\+end_src" line)
                             (setq in nil))
                           (setq l (if in line replace))
                           (when (string-match-p "^#\\+begin_src python" line)
                             (setq in t))
                           l))
                       lines)
               "\n")))

(with-eval-after-load 'anaconda-mode
  ;; Modified version of original `anaconda-mode-jsonrpc-request-data'.
  ;; This version passes the transformed contents instead.
  (defun anaconda-mode-jsonrpc-request-data (command)
    "Prepare buffer data for COMMAND call."
    `((jsonrpc . "2.0")
      (id . 1)
      (method . ,command)
      (params . ((source . ,(let ((buffer-string (buffer-substring-no-properties
						  (point-min) (point-max))))
			      (if (and (eq major-mode 'org-mode)
				       (nth 1 (org-babel-params-from-properties "python")))
				  (org-babel-python-filter buffer-string)
				buffer-string)))
		 (line . ,(line-number-at-pos (point)))
		 (column . ,(- (point) (line-beginning-position)))
		 (path . ,(pythonic-python-readable-file-name buffer-file-name)))))))

(defun org-python-eldoc-function (&rest args)
  (if (and (boundp 'anaconda-mode) anaconda-mode)
      (apply #'anaconda-mode-eldoc-function args)
    (org-python-eldoc--get-signature-at-point)))

(add-to-list 'org-eldoc-documentation-function-alist
	     '(("python") . #'org-python-eldoc-function))
{% endhighlight %}

After evaluating the code above, and then activating `anaconda-mode`,
insert a `(` directly after `os.getenv` in the code-block below, and
notice the signature hint in the echo area. Note that now it was not
necessary to evaluate the import statement first as anaconda-mode does
static analysis.

{% highlight python %}
import os
os.getenv
x = "awesome"
{% endhighlight %}

Also, place your cursor on `os` in the code block above and do `M-x
anaconda-mode-find-assignments` or `M-x
anaconda-mode-find-definitions` to navigate to the `os` module source.

Finally, place your cursor directly after `x` in the code block below
and again do `=M-x anaconda-mode-find-assignments` to find that your
cursor jumps to the location where `x` got assigned

{% highlight python %}
x
{% endhighlight %}

Unfortunately, when using `company-anaconda` we find that it also
shows python candidates outside of source blocks, for example type `.`
after the os below

os

Therefore, we have to create an 'org' alternative for the
company-anaconda backend which will show suggest python-candidates
only when the cursor is inside a python code block. For that we simple
modify the [company-anaconda](file:///home/dalanicolai/emacs-basic/elpa/29/company-anaconda-20200404.1859/company-anaconda.el) function.

{% highlight emacs-lisp %}
(defun org-company-anaconda (command &optional arg &rest _args)
  "Anaconda backend for company-mode in org buffers.
See `company-backends' for more info about COMMAND and ARG."
  (interactive (list 'interactive))
  (require 'company-anaconda)
  (cl-case command
    (interactive (company-begin-backend 'company-anaconda))
    (prefix (company-anaconda-prefix))
    (candidates (let ((element (org-element-at-point)))
		  (when (member (org-element-property :language element)
				'("python"))
		    (cons :async
			  (let ((given-prefix (s-chop-suffix (company-grab-symbol) arg)))
			    (lambda (callback)
			      (company-anaconda-candidates callback given-prefix)))))))
    (doc-buffer (company-anaconda-doc-buffer arg))
    (meta (company-anaconda-meta arg))
    (annotation (funcall company-anaconda-annotation-function arg))
    (location (company-anaconda-location arg))
    (ignore-case company-anaconda-case-insensitive)
    (sorted t)))

(autoload 'org-company-anaconda "company-anaconda")
{% endhighlight %}


## Virtual environment

Simply activating `anaconda-mode` would activate it in your global
python environment. If you would like to use `anaconda-mode` with some
virtual environment, then simply set `python-shell-virtualenv-root`
before activating `anaconda-mode`. Alhtough it would probably be no
too much work to extend the above code, and modify anaconda-mode, to
support multiple virtual environments in a buffer, here, we simply
limit the functionality to only support a single virtual environment
per org buffer.


### org hook

Finally, we could make `anaconda-mode` get activated in some virtual
environment if the org buffer defines a `:python` header-arg globally
(see top of this file).

{% highlight emacs-lisp %}
(defun org-maybe-activate-python-tools ()
  "Activate python tools if a kernel has been defined.
The kernel must be defined via a #+PROPERTY line.

This function is meant to be used via the `org-mode-hook'."
  (when-let (py-header-args (nth 1 (org-babel-params-from-properties "python")))
    (when-let (interpreter (alist-get :python py-header-args))
      (setq python-shell-virtualenv-root interpreter))

    (with-eval-after-load 'company
      (make-variable-buffer-local 'company-backends)
      ;; (add-to-list 'company-backends '(company-anaconda :with company-capf)))
      (add-to-list 'company-backends 'org-company-anaconda))
    ;; (setq-local eldoc-documentation-strategy 'eldoc-documentation-enthusiast)
    (anaconda-mode)))

(add-hook 'org-mode-hook #'org-maybe-activate-python-tools)
{% endhighlight %}

After evaluating the above source block, close and find again this
document, and type a `.` after num2words to find that anaconda-mode
now uses the correct virtual environment

{% highlight python %}
import num2words
num2words
{% endhighlight %}
