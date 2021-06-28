;;; uxntal-mode --- Major mode for uxntal code highlighting

;;; Version: 0.2.0

;;; Commentary:

;; Visit https://wiki.xxiivv.com/site/uxntal.html for more details about the language

;;; Code:

;; Add a hook so that users can run then own code when running this mode
(defvar uxntal-mode-hook nil)

;; Variables
(defvar uxntal-assembler nil)

;; Code highlighting
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

        ; Jump/Pad
        ("\\(|\\|\\$\\)[[:graph:]]+\s?" . font-lock-keyword-face)

        ; Pushing to stack
        ("\"[[:graph:]]+\s?" . font-lock-constant-face)

        ; Addressing
        ("\\.[[:graph:]]+\s?" . font-lock-preprocessor-face) ; zero-page
        (",[[:graph:]]+\s?" . font-lock-variable-name-face) ; relative
        ("\\(;\\|:\\)[[:graph:]]+\s?" . font-lock-type-face) ; absolute/raw

        ; Label/Sub-label
        ("\\(@\\|&\\)[[:graph:]]+\s?" . font-lock-function-name-face)
	
        ; Hex
        ("#[0-9a-f]+\s?" . font-lock-doc-face))
  "Uxntal highlighting based on Sublime Text syntax included in Uxn's code
https://git.sr.ht/~rabbits/uxn/tree/master/item/etc/tal.sublime-syntax.
The only extra addition is the hex highlighting")


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
    (let* ((program (substring (buffer-name) 0 -4))
	  (out (concat "/tmp/" program ".rom"))
	  (assembler (if uxntal-assembler uxntal-assembler "uxnasm")))
      
      (message (concat assembler ": assembling " (buffer-name) "..."))
      (if (= 0 (call-process assembler nil "*Uxntal*" nil (buffer-name) out))
	  (progn
	    (message (concat "Launching " out "..."))
	    (start-process program "*Uxntal*" "uxnemu" out))
	(message (concat "Failed to assemble " out))))))

(defun uxntal-insert-decimal-as-hex (arg)
  "Convert a user provided decimal to hex and insert at point"
  (interactive "sDecimal value: ")
  (insert (format "#%04x" (string-to-number arg))))

(defun uxntal-decimal-value ()
  "Message the decimal value of the current-word to the minibuffer"
  (interactive)
  (message (number-to-string (string-to-number (current-word t t) 16))))

(define-derived-mode uxntal-mode
  fundamental-mode "Uxntal"
  "Major mode for editing uxntal language code."
  (setq font-lock-string-face nil)
  (setq font-lock-defaults '(uxntal-highlights)))

;; Key Bindings
(define-key uxntal-mode-map (kbd "C-x C-e") 'uxntal-eval-buffer)
(define-key uxntal-mode-map (kbd "C-c t h") 'uxntal-insert-decimal-as-hex)
(define-key uxntal-mode-map (kbd "C-c t d") 'uxntal-decimal-value)

(provide 'uxntal-mode)
;;; uxntal-mode.el ends here
