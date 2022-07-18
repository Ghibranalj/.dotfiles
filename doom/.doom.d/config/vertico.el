;;; ../.dotfiles/doom/.doom.d/config/vertico.el -*- lexical-binding: t; -*-

(after! vertico-posframe
  (setq vertico-posframe-parameters
        '((left-fringe . 8)
          (right-fringe . 8)))
  ;; uncomment to enable vscode-like command palette (note doesn't work with eaf)
  ;; (vertico-posframe-mode 1)
  (setq vertico-posframe-poshandler 'posframe-poshandler-frame-top-center)
  )
