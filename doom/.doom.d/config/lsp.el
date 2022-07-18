;;; ../.dotfiles/doom/.doom.d/config/lsp.el -*- lexical-binding: t; -*-

(setq lsp-ui-sideline-show-diagnostics t)
(setq lsp-ui-sideline-show-hover t)
(setq lsp-ui-sideline-show-code-actions t)
(setq lsp-ui-sideline-update-mode 'line)
(add-hook! 'prog-mode-hook #'lsp-ui-mode)
(add-hook! 'lsp-mode-hook #'lsp-ui-sideline-mode)
(add-hook! 'lsp-mode-hook #'lsp-headerline-breadcrumb-mode)
(setq lsp-headerline-breadcrumb-enable t)


;; sidekick
(use-package! sidekick
  :hook (sidekick-mode . (lambda () (require 'sidekick-evil)))
  :config
  (setq sidekick-window-hide-footer t)
  (setq sidekick-window-take-focus t)
  )
