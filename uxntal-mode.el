;;; uxntal-mode --- Major mode for uxntal code highlighting

;;; Commentary:

;; Visit https://wiki.xxiivv.com/site/uxntal.html for more details about the language

;;; Code:

(setq uxntal-highlights
      '(
        ("(\\|)" . font-lock-comment-delimiter-face) ; comment delimiters
        ("(\\(.*\\))" (1 font-lock-comment-face)) ; comment content
        ("%[[:graph:]]*\\|{\\|}" . font-lock-type-face) ; macros
        ("\\(^\\|[[:blank:]]\\)\\(BRK\\|LDZ\\|LIT\\|STZ\\|---\\|LDR\\|POP\\|STR\\|DUP\\|LDA\\|SWP\\|STA\\|OVR\\|DEI\\|ROT\\|DEO\\|EQU\\|ADD\\|NEQ\\|SUB\\|GTH\\|MUL\\|LTH\\|DIV\\|JMP\\|AND\\|JCN\\|ORA\\|JSR\\|EOR\\|STH\\|SFT\\)[2r]?[2r]?" . font-lock-keyword-face) ; Opcodes
        ;("\\(^\\|[[:blank:]]*\\)\\([\\^\\$@&#\\.,;:'\"]*\\)\\([[:graph:]]*\\)\\(/\\)\\([[:graph:]]*\\)"
        ; (2 font-lock-builtin-face)
        ; (3 font-lock-variable-name-face)
        ; (4 font-lock-builtin-face)
        ; (5 font-lock-variable-name-face)) ; Runes with /
        ; ("\\(^\\|[[:blank:]]\\)\\([|\\$@&#\\.,;:'\"]*\\)\\([[:graph:]]*\\)[[:blank:]]?[[:space:]]?"
        ; (2 font-lock-builtin-face)
        ; (3 font-lock-variable-name-face)) ; Runes
        ("\\(^\\|[[:blank:]]*\\)\\([|\\$@&#\\.,;:'\"]*\\)"
         (2 font-lock-builtin-face)) ; Runes without /
        ))

(define-derived-mode uxntal-mode fundamental-mode "uxntal"
  "Major mode for editing uxntal language code."
  (setq font-lock-string-face nil)
  (setq font-lock-defaults '(uxntal-highlights)))

(provide 'uxntal)
;;; uxntal-mode.el ends here
