;;; evil-megasave-mode.el -*- lexical-binding: t; -*-

(defun evil-megasave--save-when-has-file ()
  (when (buffer-file-name)
    (save-buffer)))

(defun evil-megasave--save-unless-insert (&rest _)
  (unless (or (evil-insert-state-p) (evil-replace-state-p) (evil-emacs-state-p))
    (evil-megasave--save-when-has-file)))


;;;###autoload
(define-minor-mode evil-megasave-mode
  "Save the current buffer every time you make a change on evil-normal-state.
Also saves when you exit evil-insert-state or evil-replace-state."
  :lighter " MegaSave"
  :global nil
  (if evil-megasave-mode
      (progn
        (add-hook 'after-change-functions 'evil-megasave--save-unless-insert nil t)
        (add-hook 'evil-insert-state-exit-hook 'evil-megasave--save-when-has-file nil t)
        (add-hook 'evil-emacs-state-exit-hook 'evil-megasave--save-when-has-file nil t))
    (progn
      (remove-hook 'after-change-functions 'evil-megasave--save-unless-insert t)
      (remove-hook 'evil-insert-state-exit-hook 'evil-megasave--save-when-has-file t)
      (remove-hook 'evil-emacs-state-exit-hook 'evil-megasave--save-when-has-file t)
      ))
  )

(provide 'evil-megasave-mode)
