;;; ../.dotfiles/doom/.doom.d/config/term.el -*- lexical-binding: t; -*-

(defun remove-scratch-buffer ()
  (if (get-buffer "*scratch*")
      (kill-buffer "*scratch*")))

(if (and (daemonp) (string= (daemonp) "term"))
    (progn
      (add-hook 'after-change-major-mode-hook 'remove-scratch-buffer)
      (add-hook 'server-after-make-frame-hook 'remove-scratch-buffer)
      )
  )
