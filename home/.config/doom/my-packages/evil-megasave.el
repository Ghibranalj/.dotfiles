;;; evil-megasave-mode.el -*- lexical-binding: t; -*-
(require 'evil)

;;; Code:
(defgroup evil-megasave nil
  "Save whenever evil action is done."
  :group 'convenience)

(defun evil-megasave--save-when-has-file ()
  "Save the current buffer if it has a file name."
  (when (buffer-file-name)
    (save-buffer)))

(defun evil-megasave--save-unless-insert (&rest _)
  "Save the current buffer unless in insert state."
  (unless (or (evil-insert-state-p) (evil-replace-state-p) (evil-emacs-state-p))
    (evil-megasave--save-when-has-file)))

(defcustom evil-megasave--functions-to-hook
  '(evil-paste-after
    evil-paste-before
    evil-paste-pop
    evil-paste-pop-next
    evil-delete
    evil-delete-char
    evil-delete-line
    evil-delete-whole-line
    evil-change
    evil-change-line
    evil-undo
    evil-shift-left
    evil-shift-right
    evil-shift-left-line
    evil-shift-right-line
    evil-indent
    evil-indent-line
    evil-indent-restore)
  "Functions to hook when `evil-megasave-mode' is enabled."
  :type '(repeat function)
  :group 'evil-megasave)

;;;###autoload
(define-minor-mode evil-megasave-mode
  "Save the current buffer every time you make a change on evil-normal-state.
Also saves when you exit evil-insert-state or evil-replace-state."
  :lighter " MegaSave"
  :global nil
  (if evil-megasave-mode
      (progn
        (add-hook 'evil-insert-state-exit-hook 'evil-megasave--save-when-has-file nil t)
        (add-hook 'evil-emacs-state-exit-hook 'evil-megasave--save-when-has-file nil t)
        (dolist (func evil-megasave--functions-to-hook)
          (advice-add func :after 'evil-megasave--save-unless-insert)))
    (progn
      (remove-hook 'evil-insert-state-exit-hook 'evil-megasave--save-when-has-file t)
      (remove-hook 'evil-emacs-state-exit-hook 'evil-megasave--save-when-has-file t)
      (dolist (func evil-megasave--functions-to-hook)
        (advice-remove func 'evil-megasave--save-unless-insert)))))

(provide 'evil-megasave)
