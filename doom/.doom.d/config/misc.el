;;; ../.dotfiles/doom/.doom.d/config/mics.el -*- lexical-binding: t; -*-



(setq scroll-margin 10)

;; Scroll bar
(global-yascroll-bar-mode 1)

;; Tabs
(after! centaur-tabs
  (setq centaur-tabs-set-bar 'under
        centaur-tabs-set-close-button nil
        centaur-tabs-height 42
        ))

(after! company
  (setq company-show-quick-access t)
  (setq company-idle-delay 0)
  )

;; Beacon
(beacon-mode 1)

;; misc hook
(use-package! company-box
  :hook (company-mode . company-box-mode))

(add-hook! 'Man-mode-hook '(lambda () (persp-add-buffer (current-buffer))))
(setq projectile-indexing-method 'native)
(setq projectile-enable-caching t)

;; man pages
(setq Man-notify-method 'pushy)

(defun +my/eval-line ()
  "Evaluate the current line."
  (interactive)
  (eval-region (line-beginning-position) (line-end-position)))

(defun +my/connect-remote-ssh()
  (interactive)
  (dired (format "/ssh:%s@%s:"
                 (read-string "User: ")
                 (read-string "Host: "))))


(defun +my/comment-or-uncomment()
  "Comment or uncomment the current line or region."
  (interactive)
  (if mark-active
      (comment-or-uncomment-region (region-beginning) (region-end))
    (comment-or-uncomment-region (line-beginning-position) (line-end-position))))


(defun +my/save-current-workspace ()
  "Save current workspace."
  (interactive)
  (+workspace/save (+workspace-current-name)))
