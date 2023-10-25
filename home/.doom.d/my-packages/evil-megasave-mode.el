;;; evil-megasave-mode.el -*- lexical-binding: t; -*-
(require 'evil)

(defun evil-megasave--save-when-has-file ()
  (when (buffer-file-name)
    (save-buffer)))

(defun evil-megasave--save-unless-insert (&rest _)
  (unless (or (evil-insert-state-p) (evil-replace-state-p) (evil-emacs-state-p))
    (evil-megasave--save-when-has-file)))

(defvar evil-megasave--functions-to-hook
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
    evil-indent-restore))

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

(defun evil-megasave-add-function-to-hook (func)
  (add-to-list 'evil-megasave--functions-to-hook func)
  (if evil-megasave-mode
    (advice-add func :after 'evil-megasave--save-unless-insert)))

(provide 'evil-megasave-mode)
;;; evil-megasave-mode.el ends here
