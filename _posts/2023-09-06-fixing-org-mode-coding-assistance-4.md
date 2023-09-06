---
title: Fixing org-mode coding assistance 4
date: 2023-09-06 15:49:15 +0200
categories: [Emacs]
tags: [emacs, orgbabel]
---

{% highlight python %}
import os
os.getenv
import sys
sys.executable
import num2words
num2words.num2words(100)
{% endhighlight %}


## Coding assistance for Emacs Jupyter

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


### Test

{% highlight python %}
import os
os.getenv
import sys
sys.executable
import num2words
num2words.num
{% endhighlight %}


## Virtual environment

Now let's update our `org-maybe-activate-python-tools` function to
make it look for the `:kernel` header arg first if the jupyter package
has been activated (so that the `jupyter-kernelspec-plist` variable is
defined), and otherwise look for the `:python` arg for auto
configuring the environment for `anaconda=mode`

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

{% highlight python %}
import num2words
num2words.num
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