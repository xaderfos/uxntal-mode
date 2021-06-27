# uxntal-mode

A bare bones emacs major mode for [Uxntal](https://wiki.xxiivv.com/site/uxntal.html), the programming language of the [Uxn stack-machine](https://wiki.xxiivv.com/site/uxn.html).

I have tried to replicate the [Sublime Text highlight rules](https://git.sr.ht/~rabbits/uxn/tree/master/item/etc/tal.sublime-syntax) already included in Uxn but multiline comments are not supported yet. I have never written an emacs mode before, so please be kind <3

Contributions welcomed!

## Installation

The following configuration has been tested with a vanilla `.emacs.d`. Trying to get things working under prelude raised issues I was unable to address at the moment. 

```elisp
;; Uxntal

;;; Set location of uxntal-mode.el
(add-to-list 'load-path "~/.emacs.d/site-lisp")

;;; Make sure uxnasm and uxnemu are on your PATH if you want to use uxntal-eval-buffer
(setenv "PATH" (concat (getenv "PATH") ":/home/user/uxn/bin"))

;;; Enable the mode and associate with the .tal extension 
(require 'uxntal-mode)
(add-to-list 'auto-mode-alist '("\\.tal\\'" . uxntal-mode))
```
## Key Bindings

|binding|command|description|
|-|-|-|
|C-x C-e| uxntal-eval-buffer|Build and run the current buffer. The resulting rom is created in `/tmp/{buffer-name}.rom`|

## Screenshot

uxntal-mode with gruvbox-dark-medium theme on a vanilla Emacs installation

![uxntal-mode screenshot with misterioso theme on a vanilla Emacs installation](uxntal-mode.png)
