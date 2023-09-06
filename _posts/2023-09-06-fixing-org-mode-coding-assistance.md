---
title: Fixing org-mode coding assistance
date: 2023-09-06 12:50:30 +0200
categories: [Emacs]
tags: [emacs, orgbabel]
---


## Introduction

In this series of articles we will show how to fix dynamic and static
coding assistance for org-mode. In this article we assume that you are
using Emacs version >= 29 with org-mode version >= 9.6.7, but all
methods can also be implemented, maybe in a slightly modified form, on
lower versions of Emacs and org-mode (they should work at least from
Emacs version 27 with org-mode version 9).

The dynamic coding assistance will only work for interpreted
languages, and we will provide examples for emacs-lisp and python.

The static coding assistance within org-mode, will work for any
language that provides some static coding analysis package that
accepts buffer contents instead of files, like [jedi](https://jedi.readthedocs.io/en/latest/).

Finally, the static coding assistance in special editing buffers will
work for any language that comes with a static code analysis package
(which nowadays more or less means, any language with [LSP](https://microsoft.github.io/language-server-protocol/) support).


### This article

In this first article of the series we will fix the dynamic coding
tools for emacs-lisp. In subsequent articles we will show how to fix
dynamic and static (using some hack) coding tools for python (and
possibly other languages as well).


### Donations

As I am currently looking for work, I would be very happy with any
small (or, if you are running a company, maybe a not so small ;)
donation.  Also I am happily available for getting hired.


### Acknowledgement

The required investigations for writing this article have been made
possible by [MLP](https://www.mlprograms.com/), an very innovative AI company, who have hired me to
investigate and fix these issues for them. It has been great fun
working with them, and I am very thankful to them for providing me
this great opportunity.


## The problem

First download this article from [here](https://github.com/dalanicolai/notes/blob/main/pages/notes/fixing-org-mode-coding-assistance.org) and open it in Emacs. Then place
your cursor behind `(def` in the following source block, and press do
`M-x completion-at-point` (`C-M-i`)

{% highlight emacs-lisp %}
(def
{% endhighlight %}

Indeed you will find that nothing happens, as Emacs completion does
not work by default inside source blocks.

Now, in the above source block, complete `def` to `defun` by typing.
If you are using Emacs 29, again you will find that nothing happens,
as `eldoc` functionality does not work in code-blocks by default
(although org-eldoc which is part of [org-contrib](https://orgmode.org/worg/org-contrib/) fixes it for some
languages).


## Fixing dynamic coding assistance

The recommended way of editing source blocks, and activating dynamic
coding assistance, is by using `org-edit-special`. However, for simple
edits it is often more convenient to edit directly within the
org-buffer itself.


### Completion

Fixing dynamic completion for emacs-lisp source blocks is
straightforward. Just add the `elisp-completion-at-point` function to
your buffer-local `completion-at-point-functions`.

{% highlight emacs-lisp %}
(add-hook 'completion-at-point-functions 'elisp-completion-at-point nil t)
{% endhighlight %}

However, the above solution activates `elisp-completion-at-point`
everywhere, not just within emacs-lisp code blocks. Therefore, we will
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
(remove-hook 'completion-at-point-functions 'elisp-completion-at-point t)
(add-hook 'completion-at-point-functions 'org-completion-at-point nil t)
{% endhighlight %}

That's it, we have fixed completion within source blocks for
`emacs-lisp`. Now you could load the above function and then
automatically activate it within source blocks only as follows

{% highlight emacs-lisp %}
(add-hook 'org-mode-hook (lambda () (add-hook 'completion-at-point-functions 'org-completion-at-point nil t)))
{% endhighlight %}


### On-the-fly documentation (signature hints)

For a selection of languages, the [org-eldoc](file:///home/dalanicolai/emacs-basic/elpa/29/org-contrib-0.4.1/org-eldoc.el) package, part of
[org-contrib](https://elpa.nongnu.org/nongnu/org-contrib.html), should already provide on-the-fly documentation within
source blocks. When activating the `org-contrib` library it
automatically adds `org-eldoc-load` to the `org-mode-hook`. However,
currently, there is a [tiny but quite severe bug](https://lists.gnu.org/archive/html/emacs-orgmode/2023-05/msg00420.html) in org-eldoc which we
can fix as follows:

{% highlight emacs-lisp %}
(eldoc-add-command 'org-self-insert-command)
{% endhighlight %}

After evaluating the above source block, place your cursor on the
block its header line, and you will see header line documentation get
printed in the echo area.


### Signature hints

The org-eldoc package should already fix signature hints for
emacs-lisp source blocks, but this functionality seems broken for
Emacs 29. However we can fix it by simply commenting out the first
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
    (eldoc-print-current-symbol-info)))))
{% endhighlight %}

Voila! Now we have fixed eldoc functionality within `emacs-lisp`
source blocks also.