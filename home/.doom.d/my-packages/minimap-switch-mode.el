;;; minimap-switch-mode.el -*- lexical-binding: t; -*-
(require 'minimap)

(defgroup minimap-switch nil
  "Switch minimap to selected buffer"
  :group 'convenience)

(defcustom minimap-switch--functions-to-advise
  '(evil-window-right
    evil-window-left
    evil-window-up
    evil-window-down
    mouse-set-point
    balance-windows
    projectile-find-file)
  "List of functions to advise"
  :type '(repeat function)
  :group 'minimap-switch)

(defun minimap-switch--swap  (orig-fun &rest args)
  (minimap-mode 0)
  (unwind-protect
      (apply orig-fun args)
    (minimap-mode 1)))

;;;###autoload
(define-minor-mode minimap-switch-mode
  "Switch minimap to selected buffer"
  :lighter " Minimap-Switch"
  :global t
  (if minimap-switch-mode
      (progn
        (dolist (fun minimap-switch--functions-to-advise)
          (advice-add fun :around #'minimap-switch--swap))
        (minimap-mode 1))
    (progn
      (dolist (fun minimap-switch--functions-to-advise)
        (advice-remove fun #'minimap-switch--swap))
      (minimap-mode -1)
      (delete-window (get-buffer-window (get-buffer minimap-buffer-name)))
      (kill-buffer (get-buffer minimap-buffer-name)))))




(provide 'minimap-switch-mode)
