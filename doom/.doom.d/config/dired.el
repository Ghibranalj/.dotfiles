;;; ../.dotfiles/doom/.doom.d/config/dired.el -*- lexical-binding: t; -*-

(setq dired-listing-switches "-agho --group-directories-first")
(evil-collection-define-key 'normal 'dired-mode-map
  "h" 'dired-up-directory
  "l" 'dired-find-file
  ;; "<right>" 'dired-up-directory
  ;; "<left>" 'dired-find-file
  )
