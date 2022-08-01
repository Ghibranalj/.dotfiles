;;; evil-megasave-mode.el -*- lexical-binding: t; -*-

(require 'evil)

(defun evil-megasave--save-when-has-file ()
  (when (buffer-file-name)
    (save-buffer)))

(defun evil-megasave--save-unless-insert (&rest _)
  (unless (evil-insert-state-p)
    (evil-megasave--save-when-has-file)))


;;;###autoload
(define-minor-mode evil-megasave-mode
  "Save the current buffer everytime you make a change on evil-normal-state.
Also saves when you exit evil-insert-state"
  :lighter " MegaSave"
  :global nil
  (if evil-megasave-mode
      (progn
        (add-hook 'after-change-functions 'evil-megasave--save-unless-insert)
        (add-hook 'evil-insert-state-exit-hook 'evil-megasave--save-when-has-file))
    (progn
      (remove-hook 'after-change-functions 'evil-megasave--save-unless-insert)
      (remove-hook 'evil-insert-state-exit-hook 'evil-megasave--save-when-has-file)))
  )

(provide 'evil-megasave-mode)
