;;; uxntal-mode --- Major mode for uxntal code highlighting

;;; Commentary:

;; Visit https://wiki.xxiivv.com/site/uxntal.html for more details about the language

;;; Code:

(defvar uxntal-highlights
      '(
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

        ; Blocks
        ;: These do not support multi-line expansion yet.
        ;; Unfortunately it is unnecessarily complicated to implement it in Emacs...
        ("(\\|)\\|\\[\\|\\]" . font-lock-comment-delimiter-face) ; comment delimiters
        ("(\\(\s?TODO\s\\|\s?FIXME\s\\)\\(.*\\))" (1 font-lock-warning-face) (2 font-lock-comment-face)) ; TODO/FIXME comment
        ("(\\(.*\\))" (1 font-lock-comment-face)) ; comment content
        ("%[[:graph:]]+[[:blank:]]+{.*}" . font-lock-type-face) ; macros

        ; Hex
        ("#[0-9a-f]+\s?" . font-lock-doc-face)
        
        ))

(define-derived-mode uxntal-mode
  fundamental-mode "Uxntal"
  "Major mode for editing uxntal language code."
  (setq font-lock-string-face nil)
  (setq font-lock-defaults '(uxntal-highlights)))

(provide 'uxntal-mode)
;;; uxntal-mode.el ends here
