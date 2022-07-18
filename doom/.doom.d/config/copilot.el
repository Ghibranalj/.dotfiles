;;; ../.dotfiles/doom/.doom.d/config/copilot.el -*- lexical-binding: t; -*-


;; Coplilot
(defun +copilot/tab ()
  "Copilot completion."
  (interactive)
  (or (copilot-accept-completion)
      (indent-relative)))

(defun +copilot/tab-or-complete ()
  "Copilot completion or complete."
  (interactive)
  (or (copilot-accept-completion)
      (company-complete-common)))

(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind (("<backtab>" . 'copilot-accept-completion-by-word)
         ("<backtab>" . 'copilot-accept-completion-by-word)
         :map company-active-map
         ("<tab>" . '+copilot/tab-or-complete)
         ("TAB" . '+copilot/tab-or-complete)
         :map company-mode-map
         ("<tab>" . '+copilot/tab)
         ("TAB" . '+copilot/tab)))
