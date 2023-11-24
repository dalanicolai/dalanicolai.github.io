---
title: "Fixing org-mode coding assistance"
date: 06 September 2023
layout: post
categories: 
- Emacs
tags: 
- emacs 
- org-babel
---



## Introduction

Org mode, with its [org-babel](https://orgmode.org/worg/org-contrib/babel/intro.html) package, provides a great literate
programming environment and forms a great alternative to jupyter
notebooks. However, one thing that is currently missing when doing
literate programming in org-mode is proper coding assistance.

In this article I describe how to solve various coding assistance
issues when using org-mode for literate programming.


### Usage

This article is meant to be read interactively from inside Emacs. So
it is recommended to download the article from [here](https://raw.githubusercontent.com/dalanicolai/notes/main/pages/notes/fixing-org-mode-coding-assistance.org) and open it in
Emacs. Of course the article can be read in a browser also, but
interactive examples will not work and internal links will not be
displayed.


### Overview

This article describes how to fix dynamic and static coding assistance
for org-mode. It is assumed that you are using Emacs version >= 29
with org-mode version >= 9.6.7, but all methods can also be
implemented, maybe in a slightly modified form, on lower versions of
Emacs and org-mode (they should work at least from Emacs version 27
with org-mode version 9).

Dynamic coding assistance only works (or exists) for interpreted
languages. Examples for emacs-lisp and python are provided.

Static coding assistance within org-mode works for any language that
provides some static coding analysis package that accepts buffer
contents instead of files, like [jedi](https://jedi.readthedocs.io/en/latest/).

Finally, the static coding assistance in special editing buffers works
for any language that comes with a static code analysis package (which
nowadays more or less means, any language with [LSP](https://microsoft.github.io/language-server-protocol/) support).


### Acknowledgement

Some of the required investigations for writing this article have been
made possible by [MLP](https://www.mlprograms.com/), an innovative AI company, that has hired me to
investigate and solve some of these issues. It has been great fun
working for MLP, and I am very thankful to MLP for providing me the
great opportunity.


## Coding assistance (in this article)

As coders we are spoiled by our editors. Generally, editors provide a
lot of assistance like auto-indentation, auto-completion, signature
hints, jump to definition, code checkers, refactoring features
etc. Although these are all well known features, I could not find a
single all-encompassing term for this body of features. In this
article I refer to the collection of such basic features as coding
assistance, and usage of the term should not be confused with its
common usage for refering to more advanced [coding assistance tools](https://www.codium.ai/blog/10-best-ai-coding-assistant-tools-in-2023/)
like [copilot](https://github.com/features/copilot), [tabnine](https://www.tabnine.com/) etc.


### Dynamic and static coding assistance

Most of the coding tools consist of a front end and a back end. For
Emacs, auto-completion front ends are provided by [company](http://company-mode.github.io/), [corfu](https://github.com/minad/corfu),
[auto-complete](https://github.com/auto-complete/auto-complete) etc, the signature hints front end is provided by `eldoc`,
jump to definition by `xref`, code checking by `Flymake`&#x2026; etc. These
front ends are provided information via back ends. A full collection
of front ends is also provided by lsp clients (Eglot and LSP mode),
see [LSP](#lsp) below.


#### Dynamic coding assistance

For dynamic languages (languages with a REPL), the back end can be the
REPL. This article refers to coding assistance using such backends as
*dynamic coding assistance*. To provide coding assistance information,
the REPL needs to be aware of your code and you must evaluate your
code in the REPL (e.g. via 'send to REPL/shell' functions) before
coding assistance works. Generally such coding assistance does not
require the installation of extra packages (besides the package that
provides the REPL for your language).


#### Static coding assistance

For both static and dynamic programming languages, coding assistance
information can also be provided by 'external packages' like static
analyzers, linters, formatters etc. This article refers to such coding
assistance as *static coding assistance*.


#### LSP

Alternatively, LSP clients provide the full collection of front
ends. Emacs 29 ships with the eglot lsp-client which simply reuses the
exisiting emacs front ends mentioned in the previous paragraph. <sup><a id="fnr.1" class="footref" href="#fn.1" role="doc-backlink">1</a></sup> Another popular, and probably somewhat more
powerful, LSP client is provided by [LSP mode](https://emacs-lsp.github.io/lsp-mode/). <sup><a id="fnr.2" class="footref" href="#fn.2" role="doc-backlink">2</a></sup>


### Partial solutions

Many dynamic coding assistance issues can be solved by using
[emacs-jupyter](https://github.com/emacs-jupyter/jupyter) if possible (i.e. if a kernel for your language is
available).

Although I haven't tried it, [LSP mode is trying to implement coding
assistance support for literate programming in org-mode](https://emacs-lsp.github.io/lsp-mode/manual-language-docs/lsp-org/).


## PART 1: Fixing dynamic coding assistance for emacs-lisp

This section describes how to fix dynamic coding assistance for
emacs-lisp. Subsequent sections describe how to fix dynamic and static
(using some hack) coding assistance for python (and possibly other
languages as well).


### The problem

First download this article as descibed in the [Usage](#usage) section
above. Then place your cursor behind `(def` in the following source
block, and do `M-x completion-at-point` (`C-M-i`)

{% highlight emacs-lisp %}
(def
{% endhighlight %}

You will find that nothing happens, as emacs-lisp completion does not
work by default inside source blocks.

Now, in the above source block, complete `def` to `defun` by typing.
If you are using Emacs 29, again you will find that nothing happens,
as `eldoc` functionality does not work in code-blocks by default
(although org-eldoc which is part of [org-contrib](https://orgmode.org/worg/org-contrib/) fixes it for some
languages).


### Fixing dynamic coding assistance

The recommended way of editing source blocks with dynamic coding
assistance, is by using `org-edit-special`. However, for simple edits it
is often more convenient to edit directly within the org-buffer
itself.


#### Completion

Fixing dynamic completion for emacs-lisp source blocks is
straightforward. Just add the `elisp-completion-at-point` function to
your buffer-local `completion-at-point-functions`.

{% highlight emacs-lisp %}
(add-hook 'completion-at-point-functions 'elisp-completion-at-point nil t)
{% endhighlight %}

However, the above solution activates `elisp-completion-at-point`
everywhere, not just within emacs-lisp code blocks. Therefore, we
borrow a trick from [org-eldoc](file:///home/dalanicolai/emacs-basic/elpa/29/org-contrib-0.4.1/org-eldoc.el) (part of [org-contrib](https://elpa.nongnu.org/nongnu/org-contrib.html)) to limit its
activation to the regions within emacs-lisp code blocks

{% highlight emacs-lisp %}
(defun org-completion-at-point ()
  (let ((element (org-element-at-point)))
    (when (member (org-element-property :language element)
		  '("emacs-lisp" "elisp"))
      (funcall #'elisp-completion-at-point))))
{% endhighlight %}

{% highlight emacs-lisp %}
(add-hook 'completion-at-point-functions 'org-completion-at-point nil t)
{% endhighlight %}

That's it, this has fixed completion within source blocks for
`emacs-lisp`. Check it by placing you cursor after `(def` in the
following source block and pressing `C-M-i`:

{% highlight emacs-lisp %}
(def
{% endhighlight %}

Load the above function and automatically activate it within source
blocks only, as follows:

{% highlight emacs-lisp %}
(add-hook 'org-mode-hook (lambda ()
 (add-hook 'completion-at-point-functions 'org-completion-at-point nil t)))
{% endhighlight %}


#### On-the-fly documentation (signature hints)

For a selection of languages, the [org-eldoc](file:///home/dalanicolai/emacs-basic/elpa/29/org-contrib-0.4.1/org-eldoc.el) package, part of
[org-contrib](https://elpa.nongnu.org/nongnu/org-contrib.html), should already provide on-the-fly documentation within
source blocks (the rest of this article assumes that the [org-contrib](https://elpa.nongnu.org/nongnu/org-contrib.html)
is available). When activating the `org-contrib` library it
automatically adds `org-eldoc-load` to the `org-mode-hook`. However,
currently, there is a [tiny but quite severe bug](https://lists.gnu.org/archive/html/emacs-orgmode/2023-05/msg00420.html) in org-eldoc which can
be fixed as follows:

{% highlight emacs-lisp %}
(eldoc-add-command 'org-self-insert-command)
{% endhighlight %}

After installing org-contrib and evaluating the above source block,
place your cursor on the block its header line, and you will see
header line documentation getting printed in the echo area.


#### Signature hints

The org-eldoc package should already fix signature hints for
emacs-lisp source blocks, but this functionality seems broken for
Emacs 29. However this can be fixed by simply commenting out the first
clause in the 'cond' of the emacs-lisp/elisp case in the [original
org-eldoc-documentation-function](file:///home/dalanicolai/emacs-basic/elpa/29/org-contrib-0.4.1/org-eldoc.el) as follows (and reloading the
function, try it :)

{% highlight emacs-lisp %}
(cond
 ;; ((and (boundp 'eldoc-documentation-functions) ; Emacs>=28
 ;;       (fboundp 'elisp-eldoc-var-docstring)
 ;;       (fboundp 'elisp-eldoc-funcall))
 ;;  (let ((eldoc-documentation-functions
 ;;         '(elisp-eldoc-var-docstring elisp-eldoc-funcall)))
 ;;    (eldoc-print-current-symbol-info)))
 ((fboundp 'elisp-eldoc-documentation-function)
  (elisp-eldoc-documentation-function))
 (t            ; Emacs<25
  (let (eldoc-documentation-function)
    (eldoc-print-current-symbol-info))))
{% endhighlight %}

Voila! This has also fixed eldoc functionality within `emacs-lisp`
source blocks.


## PART 2: Fixing python dynamic coding assistance

The previous section showed how to fix dynamic coding assistance
within org-mode emacs-lisp source code blocks. The current section
shows how to fix dynamic coding assistance for python source blocks,
including support for working with virtual environments.


### Completion

The `org-eldoc` trick from the previous section can also be used to
fix auto-completion inside python source blocks. However, it is handy
to modify somewhat the `org-completion-at-point` funtion to make it
easily extensible to other languages via a
`org-completion-functions-alist`:

{% highlight emacs-lisp %}
(defun org-completion-at-point ()
  (let* ((element (org-element-at-point))
	 (lang (org-element-property :language element)))
    (when-let (fn (alist-get lang org-completion-functions-alist
			     nil nil #'string=))
      (funcall fn))))
{% endhighlight %}

The `python-completion-at-point` function fetches completion candidates
from the inferior process buffer. So python completion requires a
running inferior process, hence it is required to include a `:session`
header argument. In this article we set the header argument for the
document via the `#+PROPERTY` keyword at the [top of the document](https://raw.githubusercontent.com/dalanicolai/dalas-playground/main/_posts/org/2023-09-06-fixing-org-mode-coding-assistance.org). You
can optionally configure default header arguments via the
`org-babel-default-header-args:python` variable. Finally, inspired by
`python-completion-at-point`, a custom
`org-python-completion-at-point` function fetches completions from the
session its inferior process buffer. Like in the previous section it
can be activated via a local hook (the `org-completion-at-point` makes
sure that it only completes inside source-blocks=):

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
sys
{% endhighlight %}


### On-the-fly documentation (signature hints)

Note the fix of [this bug](https://lists.gnu.org/archive/html/emacs-orgmode/2023-05/msg00420.html) in the [On-the-fly documentation (signature hints)](#on-the-fly-documentation-signature-hints) section.

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

Now a python documentation function, this time inspired by the
`python-eldoc-function`, which, like the python completion function,
requires a running session, could be created and added to the
`org-eldoc-documentation-function-alist`. However, by default, the
`python-eldoc-function` does not return signatures. Its behavior could
be changed via the `python-eldoc-setup-code`, but here it is changed
by creating separate variable to only deviate from the default
behavior in org-mode buffers

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
that Emacs now shows signature hints when placing your cursor after
`chdir` in the second block below (also try typing a `(` after it)

{% highlight python %}
import os
{% endhighlight %}

{% highlight python %}
os.chdir
{% endhighlight %}


### Virtual environments

If you would like the session to use some virtual environment then
configure the virtual environments interpreter via the `:python`
keyword.

To test it, first create a virtual environment for example by
evaluating the following code block

{% highlight sh %}
mkdir -p ~/.virtualenvs
cd ~/.virtualenvs
python -m venv testenv
source testenv/bin/activate
pip install num2words
{% endhighlight %}

After evaluating the above code block and the first source block
below, type `2` after `num2words.num` in the second source block below
and press `C-M-i`. Then type a `(` to find that eldoc functionality
works also

{% highlight python %}
import num2words
{% endhighlight %}

{% highlight python %}
num2words.num2
{% endhighlight %}

**Note**
Obviously, different environments can not share the same sessions


## Implementing org static coding assistance with anaconda-mode

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
company-anaconda backend which will suggest python-candidates only
when the cursor is inside a python code block. For that we simple
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
before activating `anaconda-mode`. Alhtough it would probably be not
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


## PART 3: Coding assistance for Emacs Jupyter

For languages with Jupyter support, the easiest way to work with
source blocks is by using [emacs-jupyter](https://github.com/emacs-jupyter/jupyter). Its README file explains how
to use it. This package comes with completion and documentation
(although not on-the-fly) inside source blocks and special edit
buffers by default. Here we assume that you are using
`(org-babel-jupyter-override-src-block "python")`

When using the 'override', the main difference with using python
directly is that the `:python` header arg has no effect. Instead
jupyter expects a `:kernel` header arg. For python, you can read
[here](https://ipython.readthedocs.io/en/stable/install/kernel_install.html) how to install a kernel in some virtual environment.

{% highlight sh %}
source ~/.virtualenvs/testenv/bin/activate
pip install ipykernel
python -m ipykernel install --user --name testenv
{% endhighlight %}

{% highlight emacs-lisp %}
(jupyter-available-kernelspecs t)
{% endhighlight %}


## Virtual environment

Now let's update our `org-maybe-activate-python-tools` function to
make it look for the `:kernel` header arg first if the jupyter package
has been activated (so that the `jupyter-kernelspec-plist` variable is
defined), and otherwise look for the `:python` arg for auto
configuring the environment for `anaconda-mode`

{% highlight emacs-lisp %}
(defun org-maybe-activate-python-tools ()
  "Activate python tools if a kernel has been defined.
The kernel must be defined via a #+PROPERTY line.

This function is meant to be used via the `org-mode-hook'."
  (when-let (py-header-args (nth 1 (org-babel-params-from-properties "python")))
    (when-let ((interpreter (if-let (kernel (and (fboundp 'jupyter-kernelspec-plist)
						 (alist-get :kernel py-header-args)))
				(elt (plist-get (jupyter-kernelspec-plist (jupyter-get-kernelspec kernel))
						:argv)
				     0)
			      (alist-get :python py-header-args))))
      (setq python-shell-virtualenv-root interpreter))


    (with-eval-after-load 'company
      (make-variable-buffer-local 'company-backends)
      ;; (add-to-list 'company-backends '(company-anaconda :with company-capf)))
      (add-to-list 'company-backends 'org-company-anaconda))
    ;; (setq-local eldoc-documentation-strategy 'eldoc-documentation-enthusiast)
    (anaconda-mode)))

(add-hook 'org-mode-hook #'org-maybe-activate-python-tools)
{% endhighlight %}

Now close and find again this document and insert a `.` after
`num2words` in the source block below to find that `anaconda-mode`
now uses the correct virtual environment

{% highlight jupyter-python %}
import num2words
num2words
{% endhighlight %}


### Python/Jupyter compatibility

Although we have fixed the static coding assistance to always use the
correct virtual environment (provided that `emacs-jupyter` has been
activated), we did not yet fix it for the (evaluation of the) source
blocks themselves (for example after evaluating
`(org-babel-jupyter-restore-src-block "python")`). Therefore, let's
fix the `org-babel-execute:python` function to always use the
'correct' virtual environment

{% highlight emacs-lisp %}
(defun org-babel-execute:python (body params)
  "Execute a block of Python code with Babel.
This function is called by `org-babel-execute-src-block'."
  (let* ((org-babel-python-command
	  ;; here we duplicate the 'if-let' from
	  ;; 'org-maybe-activate-python-tools', we could also decide
	  ;; to extract it into a separete function
	  (or (if-let (kernel (and (fboundp 'jupyter-kernelspec-plist)
				   (alist-get :kernel py-header-args)))
		  (elt (plist-get (jupyter-kernelspec-plist (jupyter-get-kernelspec kernel))
						:argv)
		       0)
		(alist-get :python params))
	      org-babel-python-command))
	 (session (org-babel-python-initiate-session
		   (cdr (assq :session params))))
         (result-params (cdr (assq :result-params params)))
         (result-type (cdr (assq :result-type params)))
	 (return-val (when (eq result-type 'value)
		       (cdr (assq :return params))))
	 (preamble (cdr (assq :preamble params)))
	 (async (org-babel-comint-use-async params))
         (full-body
	  (concat
	   (org-babel-expand-body:generic
	    body params
	    (org-babel-variable-assignments:python params))
	   (when return-val
	     (format (if session "\n%s" "\nreturn %s") return-val))))
         (result (org-babel-python-evaluate
		  session full-body result-type
		  result-params preamble async)))
    (org-babel-reassemble-table
     result
     (org-babel-pick-name (cdr (assq :colname-names params))
			  (cdr (assq :colnames params)))
     (org-babel-pick-name (cdr (assq :rowname-names params))
			  (cdr (assq :rownames params))))))
{% endhighlight %}


## PART 4: Fixing coding assistance in special editing buffers

In the previous articles we have fixed 'in-org' coding
assistance. However, except for the smallest edits, editing is better
done in a special editing buffer. Therefore, in this article we will
fix coding assistance in special editing buffers.


### Dynamic coding assistance

The `emacs-jupyter` package, provides dynamic coding assistance for
special editing buffers by default. To fix coding assistance in the
special edit buffer, just associate the buffer with the correct python
project via the `org-babel-edit-prep:python` function

{% highlight emacs-lisp %}
(defun org-babel-edit-prep:python (info)
  (setq python-shell-buffer-name (alist-get :session (nth 2 info))))
{% endhighlight %}


### Static coding assistance

To get static coding assistance in the special editing buffer, we
could just configure some static coding assistance tool to get
activated automatically in buffers os the major mode associated with
the language. However, when using the static analysis tool in special
editing buffers, a problem is that the tool will only be aware of the
code contents in the special editing buffer without context, i.e. only
the code of the current source block being edited and not that of
other source blocks.

A nice solution to make the special editing buffer 'context aware' has
been proposed by the user `karthink` [here](https://list.orgmode.org/87bkqmdhqz.fsf@gmail.com/). Although that solution is
probably not yet ready to get merged into org-mode, it is already
usable and does it's job well.


#### Caveats

There are a few caveats when using karthink's [org-src-context.el](https://github.com/karthink/org-src-context/blob/master/org-src-context.el)
solution:

-   due to the 'limited' functionality of the
    `org-babel-tangle-collect-blocks` function, `org-src-context.el` does
    not add collect context by `:session` but by `:tangle`. Therefore,
    if working with multiple sections, you should additionally use
    `:tangle` arguments to 'indicate/control' which blocks belong to
    which context. When using `org-src-context.el` to edit a block
    without `:tangle` argument, then it includes all code blocks of the
    same language as 'context'.
-   although 'jump to definition' works fine for definitions not part of
    the buffer, it works a bit problematic for definitions (things
    defined) within the same buffer

1.  also

    -   when using tangle detangle -> duplicate headings
    
    {% highlight emacs-lisp %}
    (defun org-edit-src-tangle ()
      (interactive)
      (let* ((headline (org-get-heading t t t t))
    	 (line (print (thing-at-point 'line t)))
    	 (column (current-column))
    	 (info (org-babel-get-src-block-info 'no-eval))
    	 (params (nth 2 info))
    	 (offset (count-lines (nth 5 info) (point)))
    	 (session (cdr (assq :session params)))
             (jupyterp (advice--p (advice--symbol-function 'org-babel-execute:python)))
             (client-buffer (if jupyterp
    			    (org-babel-jupyter-initiate-session session args)
    			  (org-babel-python-initiate-session session)))
    	 (tangle-dest (alist-get :tangle params)))
        (org-babel-tangle '(16) nil "python")
        (find-file tangle-dest)
        (if jupyterp
            (jupyter-repl-associate-buffer client-buffer) ;from org-babel-edit-prep:jupyter
          (setq python-shell-buffer-name (substring client-buffer 1 -1)))
        (when headline
          (search-forward (concat "*" headline)))
        (search-forward line)
        (goto-char (match-beginning 0))
        (recenter offset)
        (forward-char column)))
    {% endhighlight %}
    
    {% highlight emacs-lisp %}
    ((nil (eglot-workspace-configuration
           :pylsp (:plugins
                   (:jedi
    		(:environment "~/.virtualenvs/testenv"))))))
    {% endhighlight %}

# Footnotes

<sup><a id="fn.1" href="#fnr.1">1</a></sup> This was mentioned somewhere in the eglot contribution notes, but I
can not find it anymore

<sup><a id="fn.2" href="#fnr.2">2</a></sup> You can find a
discussion about some differences at
<https://github.com/joaotavora/eglot/issues/180>
