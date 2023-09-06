---
title: Fixing org-mode coding assistance 5
date: 2023-09-6 15:50:40 +0200
categories: [Emacs]
tags: [emacs, orgbabel]
---


## Introduction

In the previous articles we have fixed 'in-org' coding
assistance. However, except for the smallest edits, editing is better
done in a special editing buffer. Therefore, in this article we will
fix coding assistance in special editing buffers.


## Fixing coding assistance in special editing buffers


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