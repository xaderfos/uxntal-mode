# uxntal-mode

A bare bones emacs major mode for [Uxntal](https://wiki.xxiivv.com/site/uxntal.html), the programming language of the [Uxn stack-machine](https://wiki.xxiivv.com/site/uxn.html).

I have tried to replicate the [Sublime Text highlight rules](https://git.sr.ht/~rabbits/uxn/tree/master/item/etc/tal.sublime-syntax) already include in Uxn.

Multiline comments are not supported yet (even though I'm not sure if they should be anyway?)

I have never written an emacs mode before, so please be kind <3

Contributions welcomed!

## Installation

The following configuration has been tested with a vanilla `.emacs.d`. Trying to get things working under prelude raised issues I was unable to address at the moment. 

```elisp
;; Uxntal

;;; Assuming uxntal-mode.el is placed under the following path
(add-to-list 'load-path "~/.emacs.d/site-lisp")
(require 'uxntal-mode)
(add-to-list 'auto-mode-alist '("\\.tal\\'" . uxntal-mode))
```

## Screenshot

uxntal-mode with misterioso theme on a vanilla Emacs installation

![uxntal-mode screenshot with misterioso theme on a vanilla Emacs installation](uxntal-mode.png)