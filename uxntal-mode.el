;;; uxntal-mode --- Major mode for uxntal code highlighting

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
        ("#[0-9a-f]+\s?" . font-lock-doc-face)
        
        ))

(defun uxntal-eval-buffer ()
  (interactive)
  (when buffer-file-name
    (let ((out (concat "/tmp/" (substring (buffer-name) 0 -4) ".rom")))
      (message (concat "building " (buffer-name) "..."))
      (shell-command (concat "uxnasm " (buffer-file-name) " " out " >/dev/null") "*Uxn*")
      (message (concat "launching " out "..."))
      (shell-command (concat "uxnemu " out " &") "*Uxn*")
      )))

(add-to-list 'display-buffer-alist '("*Uxn*" display-buffer-no-window (nil)))

(define-derived-mode uxntal-mode
  fundamental-mode "Uxntal"
  "Major mode for editing uxntal language code."
  (setq font-lock-string-face nil)
  (setq font-lock-defaults '(uxntal-highlights)))

(define-key uxntal-mode-map (kbd "C-x C-e") 'uxntal-eval-buffer)

(provide 'uxntal-mode)
;;; uxntal-mode.el ends here
