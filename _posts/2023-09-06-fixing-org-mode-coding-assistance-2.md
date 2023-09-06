---
title: Fixing org-mode coding assistance 2
date: 2023-09-06 14:25:30 +0200
categories: [Emacs]
tags: [emacs, orgbabel]
---

In [the previous article](file:///home/dalanicolai/git/notes/pages/notes/fixing-org-mode-coding-assistance.md), we showed how to fix dynamic coding
assistance within org-mode emacs-lisp source code blocks. In this
article we will fix dynamic coding assistance for python source
blocks, including support for working with virtual environments.


## Fixing python dynamic coding assistance


### Completion

We use the same `org-eldoc` trick as in the previous article, but we
slightly modify the `org-completion-at-point` function to make it
easily extensible to other languages via a
`org-completion-functions-alist`.

{% highlight emacs-lisp %}
(defun org-completion-at-point ()
  (let* ((element (org-element-at-point))
	 (lang (org-element-property :language element)))
    (when-let (fn (alist-get lang org-completion-functions-alist
			     nil nil #'string=))
      (funcall fn))))
{% endhighlight %}

Then, we find from the `python-completion-at-point` function that it
fetches completion candidates from the inferior process buffer. So
python completion requires a running inferior process, hence it is
required to use the include a `:session` header argument. You can
optionally configure default header arguments via the
[org-babel-default-header-args:python](org-babel-default-header-args:python) variable. Finally, inspired by
`python-completion-at-point`, we only need to create a completion
function `org-python-completion-at-point`) that fetches completions
from the session inferior process buffer. Like in the previous article
we also add the hook to activate completion only withing org-mode
source blocks

{% highlight emacs-lisp %}
(defvar org-completion-functions-alist
  '(("emacs-lisp" . elisp-completion-at-point)
    ("python"     . org-python-completion-at-point))
  "Alist for configuring language completion functions.")


(defun org-python-completion-at-point ()
  "For org-mode modified version of `python-completion-at-point'."
  (let* ((info (org-babel-get-src-block-info))
	 (session (alist-get :session (nth 2 info)))
	 (buffer (get-buffer (org-babel-python-with-earmuffs session)))
	 (process (get-buffer-process buffer)))
    (when (and process
	       (with-current-buffer buffer
                 (python-util-comint-end-of-output-p)))
      (python-shell-completion-at-point process))))

(add-hook 'org-mode-hook (lambda ()
			   (add-hook 'completion-at-point-functions 'org-completion-at-point nil t)))
{% endhighlight %}

Now find that, **after evaluating the first source block** below,
completion is working in the second source block below, by typing in
that second block a `.` after `sys`, followed by pressing `C-M-i`

{% highlight python %}
import sys
{% endhighlight %}

{% highlight python %}
sys.executable
{% endhighlight %}


### On-the-fly documentation (signature hints)

We already fixed [this bug](https://lists.gnu.org/archive/html/emacs-orgmode/2023-05/msg00420.html) in [the previous article](file:///home/dalanicolai/git/notes/pages/notes/fixing-org-mode-coding-assistance.md).

The org-eldoc package should already fix on-the-fly documentation in
source blocks for several languages, but by default no support is
included for python. However, the [original
org-eldoc-documentation-function](file:///home/dalanicolai/emacs-basic/elpa/29/org-contrib-0.4.1/org-eldoc.el) function does not allow for easy
extension to different languages. Therefore, we first modify the
`org-eldoc-documentation-function`, and make it extensible via an
`org-eldoc-documentation-function-alist`:

{% highlight emacs-lisp %}
(defvar org-eldoc-documentation-function-alist
  '((("org"))
    (("emacs-lisp" "elisp") elisp-eldoc-documentation-function)
    (("c" "C") . (when (require 'c-eldoc nil t)
		   (c-eldoc-print-current-symbol-info)))
    (("css") . (when (require 'css-eldoc nil t)
                 (css-eldoc-function)))
    (("php") . (when (require 'php-eldoc nil t)
                 (php-eldoc-function)))
    (("go" "golang") . (when (require 'go-eldoc nil t)
			 (go-eldoc--documentation-function))))
  "Alist of expressions to call for different languages.
The functions need to be suitable for use in org-mode. Usually it
is quite straightforward to create a modified version from the
language its original eldoc function..")

(defun org-eldoc-documentation-function (&rest args)
  "Modified version of the original eldoc.el version.
This version allows for easily adding support for other languages
via the `org-eldoc-documentation-function-alist'.

THIS FUNCTION LACKS THE 'ELSE' CLAUSE"
  (or
   (org-eldoc-get-breadcrumb)
   (org-eldoc-get-src-header)
   (when-let (sexp (cdr (seq-find (lambda (c)
				    (member (org-eldoc-get-src-lang) (car c)))
				  org-eldoc-documentation-function-alist)))
     (when sexp
       (eval sexp)))))
{% endhighlight %}

Now we can create a python documentation function, this time inspired
by the `python-eldoc-function` which, like the python completion
function, requires a running session, and add it to the
`org-eldoc-documentation-function-alist`. However, by default, the
`python-eldoc-function` does not return signatures. We can change its
behavior via the `python-eldoc-setup-code`, but here we will create a
separate variable to only deviate from the default behavior in
org-mode buffers

{% highlight emacs-lisp %}
(defvar org-python-eldoc-setup-code
  "def __PYDOC_get_help(obj):
    try:
        import inspect
        try:
            str_type = basestring
            argspec_function = inspect.getargspec
        except NameError:
            str_type = str
            argspec_function = inspect.getfullargspec
        if isinstance(obj, str_type):
            obj = eval(obj, globals())
        doc = inspect.signature(obj)
    except:
        doc = ''
    return str(doc)"
  "For org-mode modified version of `python-eldoc-setup-code'.
This version retrieves signature hints instead of description
hints.")

(defun org-python-eldoc--get-signature-at-point ()
  "For org-mode modified version of `python-eldoc--get-doc-at-point'.
This version uses `org-python-eldoc-setup-code' which, by
default, retrieves a signature (instead of docstring)."
  ;; narrow to block is required for
  ;; `python-eldoc--get-symbol-at-point' to work reliably
  (save-restriction
    (org-narrow-to-block)
    (let* ((input (python-eldoc--get-symbol-at-point))
	   (info (org-babel-get-src-block-info))
	   (session (alist-get :session (nth 2 info)))
	   (buffer (get-buffer (org-babel-python-with-earmuffs session)))
	   (process (get-buffer-process buffer))
	   (docstring
            (when (and input process)
              ;; Prevent resizing the echo area when iPython is
              ;; enabled.  Bug#18794.
              (python-util-strip-string
               (python-shell-send-string-no-output
		(format
		 "%s\nprint(__PYDOC_get_help(%s))"
		 org-python-eldoc-setup-code
		 ;; "help(%s)\n"
		 (python-shell--encode-string input))
		process)))))
      (unless (string-empty-p docstring)
	docstring))))

(add-to-list 'org-eldoc-documentation-function-alist
	     '(("python") org-python-eldoc--get-signature-at-point))
{% endhighlight %}

After evaluating the source blocks above (including the code to fix
the org-eldoc bug) and evaluating the first block below, you will find
that Emacs now shows signature hints when placing you curseo after
`chdir` in the second block below (also try typing a `(` after it)

{% highlight python %}
import os
{% endhighlight %}

{% highlight python %}
os.chdir
{% endhighlight %}


## Virtual environments

If you would like the session to use some virtual environment then
configure the virtual environments interpreter via the `:python`
keyword, e.g.

To test it, first create a virtual environment for example by
evaluating the following code block

{% highlight sh %}
mkdir -p ~/.virtualenvs
cd ~/.virtualenvs
python -m venv testenv
source testenv/bin/activate
pip install num2words
{% endhighlight %}

After evaluating the above code block, type `2` after `num2words.num`
in the second codeblock below and press `C-M-i`. Then type a `(` to
find that eldoc functionality works also

{% highlight python %}
import num2words
{% endhighlight %}

{% highlight python %}
num2words.num
{% endhighlight %}

**Note**
Obviously, different environments can not share the same sessions
