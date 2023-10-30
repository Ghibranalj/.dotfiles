;;; xmodmap-mode.el --- Major mode for editing xmodmap files -*- lexical-binding: t; -*-
(defgroup xmodmap-mode nil
  "Major mode for editing xmodmap files."
  :group 'languages
  :prefix "xmodmap-")

(defcustom xmodmap-keywords
  '("add" "clear" "keycode" "keysym" "pointer" "remove")
  "Keywords for xmodmap mode."
  :type '(repeat string)
  :group 'xmodmap-mode)

(defcustom xmodmap-font-lock-keywords
  '(("add\\|clear\\|keycode\\|keysym\\|pointer\\|remove" . font-lock-keyword-face)
    ("NoSymbol" . font-lock-constant-face)
    ("!*$\\|?*$" . font-lock-comment-face))
  "Highlighting expressions for xmodmap mode."
  :type '(alist :key-type string :value-type face)
  :group 'xmodmap-mode)

(defcustom xmodmap-filename-regex
  "\\.?[xX]modmap\\(rc\\)?\\'"
  "Regex for xmodmap files."
  :type 'regexp
  :group 'xmodmap-mode
  :set (lambda (symbol value)
         (set-default symbol value)
         (add-to-list 'auto-mode-alist `(,value . xmodmap-mode))))

(defun xmodmap-completion-at-point ()
  "This is the function to be used for the hook `completion-at-point-functions'."
  (interactive)
  (let* (
         (bds (bounds-of-thing-at-point 'symbol))
         (start (car bds))
         (end (cdr bds)))
    (list start end xmodmap-keywords . nil )))

;;;###autoload
(define-derived-mode xmodmap-mode prog-mode "Xmodmap"
  (setq font-lock-defaults '(xmodmap-font-lock-keywords))
  ;; completion
  (add-hook 'completion-at-point-functions 'xmodmap-completion-at-point nil t)
  ;; comment starts with either ! or ?
  (modify-syntax-entry ?! "< b" xmodmap-mode-syntax-table)
  (modify-syntax-entry ?\n "> b" xmodmap-mode-syntax-table)
  (modify-syntax-entry ?\? "< b" xmodmap-mode-syntax-table)
  (modify-syntax-entry ?\n "> b" xmodmap-mode-syntax-table))

;; automode alist
(add-to-list 'auto-mode-alist `(,xmodmap-filename-regex . xmodmap-mode))

(provide 'xmodmap-mode)
