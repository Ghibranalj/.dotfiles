;;; minimap-switch-mode.el -*- lexical-binding: t; -*-

(defun minimap-switch--swap  (orig-fun &rest args)
  (minimap-mode 0)
  (unwind-protect
      (apply orig-fun args)
    (if minimap-switch-mode
        (minimap-mode 1) )
    ))

;;;###autoload
(define-minor-mode minimap-switch-mode
  "Switch minimap to selected buffer"
  :lighter " Minimap-Switch"
  :global t
  (if minimap-switch-mode
      (progn
        (advice-add 'evil-window-right :around 'minimap-switch--swap)
        (advice-add 'evil-window-left :around 'minimap-switch--swap)
        (advice-add 'evil-window-up :around 'minimap-switch--swap)
        (advice-add 'evil-window-down :around 'minimap-switch--swap)
        (advice-add 'mouse-set-point :around 'minimap-switch--swap)
        (advice-add 'balance-windows :around 'minimap-switch--swap)
        (advice-add 'projectile-find-file :around 'minimap-switch--swap)
        (minimap-mode 1))
    (progn
      (advice-remove 'evil-window-right 'minimap-switch--swap)
      (advice-remove 'evil-window-left  'minimap-switch--swap)
      (advice-remove 'evil-window-up  'minimap-switch--swap)
      (advice-remove 'evil-window-down  'minimap-switch--swap)
      (advice-remove 'mouse-set-point  'minimap-switch--swap)
      (advice-remove 'balance-windows  'minimap-switch--swap)
      (advice-remove 'projectile-find-file  'minimap-switch--swap)
      (minimap-mode 0)
      )))

(provide 'minimap-switch-mode)
