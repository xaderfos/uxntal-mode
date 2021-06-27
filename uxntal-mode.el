;;; uxntal-mode --- Major mode for uxntal code highlighting

;;; Version: 0.1.0

;;; Commentary:

;; Visit https://wiki.xxiivv.com/site/uxntal.html for more details about the language

;;; Code:

(defvar uxntal-highlights
  '(
        ; Blocks
        ;: These do not support multi-line expansion yet.
        ;; Unfortunately it is unnecessarily complicated to implement it in Emacs...
        ("(\\|)\\|\\[\\|\\]" . font-lock-comment-delimiter-face) ; comment delimiters
        ("(\\(\s?TODO\s\\|\s?FIXME\s\\)\\(.*\\))" (1 font-lock-warning-face) (2 font-lock-comment-face)) ; TODO/FIXME comment
        ("(\\(.*\\))" (1 font-lock-comment-face)) ; comment content
        ("%[[:graph:]]+[[:blank:]]+{.*}" . font-lock-type-face) ; macros

        ; Get
        ("\\.[[:graph:]]+\s\\(DEI\\|LDZ\\)2?" . font-lock-builtin-face)
        (",[[:graph:]]+\s\\(LDR\\)2?" . font-lock-builtin-face)
        (";[[:graph:]]+\s\\(STA\\)2?" . font-lock-builtin-face)

        ; Set
        ("\\.[[:graph:]]+\s\\(DEO\\|STZ\\)2?" . font-lock-builtin-face)
        (",[[:graph:]]+\s\\(STR\\)2?" . font-lock-builtin-face)
        (";[[:graph:]]+\s\\(LDA\\)2?" . font-lock-builtin-face)

        ; Label/Sub-label
        ("\\(@\\|&\\)[[:graph:]]+\s?" . font-lock-function-name-face)

        ; Jump/Pad
        ("\\(|\\|\\$\\)[[:graph:]]+\s?" . font-lock-keyword-face)

        ; Pushing to stack
        ("\"[[:graph:]]+\s?" . font-lock-constant-face)

        ; Addressing
        ("\\.[[:graph:]]+\s?" . font-lock-preprocessor-face) ; zero-page
        (",[[:graph:]]+\s?" . font-lock-variable-name-face) ; relative
        ("\\(;\\|:\\)[[:graph:]]+\s?" . font-lock-type-face) ; absolute/raw

        ; Hex
        ("#[0-9a-f]+\s?" . font-lock-doc-face))
  "Uxntal highlighting based on Sublime Text syntax included in Uxn's code
https://git.sr.ht/~rabbits/uxn/tree/master/item/etc/tal.sublime-syntax.
The only extra addition is the hex highlighting")

;; Add a hook so that users can run then own code when running this mode
(defvar uxntal-mode-hook nil)


;; Autoload mode for .tal buffers
(add-to-list 'auto-mode-alist '("\\.tal\\'" . uxntal-mode))

;; Keep assembler and emulator output in the background
(add-to-list 'display-buffer-alist '("*Uxntal*" display-buffer-no-window (nil)))


;; NOTE: Check this for exec-path and PATH issues https://www.emacswiki.org/emacs/ExecPath
(defun uxntal-eval-buffer ()
  "Assemble and run the current buffer's file (it will miss any unsaved changes).
TODO improve this so that it actually processes the buffer and not the file"
  (interactive)
  (when buffer-file-name
    (let ((out (concat "/tmp/" (substring (buffer-name) 0 -4) ".rom")))
      (message (concat "Assembling " (buffer-name) "..."))
      (if (= 0 (call-process "uxnasm" nil "*Uxntal*" nil (buffer-name) out))
      (progn (message (concat "Launching " out "..."))
	     (shell-command (concat "uxnemu " out " &") "*Uxntal*"))
      (message (concat "Failed to assemble " out))))))


(define-derived-mode uxntal-mode
  fundamental-mode "Uxntal"
  "Major mode for editing uxntal language code."
  (setq font-lock-string-face nil)
  (setq font-lock-defaults '(uxntal-highlights)))

;; Key Bindings
(define-key uxntal-mode-map (kbd "C-x C-e") 'uxntal-eval-buffer)


(provide 'uxntal-mode)
;;; uxntal-mode.el ends here
