;;; ../.dotfiles/doom/.doom.d/config.el -*- lexical-binding: t; -*-

(defvar +my/new-frame-hook nil
  "Hook run after a new frame is created.")

(defun on-new-frame ()
  "This is executed when a new frame is created."
  (run-hooks '+my/new-frame-hook)
  )
;; Running on daemon startup
(if (daemonp)
    (add-hook 'after-make-frame-functions (lambda (frame)
                                            (with-selected-frame frame
                                              (on-new-frame))))
  (on-new-frame))


;; Load configs inside config subdirectory
(dolist (config '(
                  ;; "vertico.el"
                  "autosave.el"
                  "consult.el"
                  "copilot.el"
                  "dired.el"
                  "doom.el"
                  "eaf.el"
                  "keymap.el"
                  "lsp.el"
                  "magit.el"
                  "minimap.el"
                  "misc.el"
                  "term.el"
                  "treemacs.el"
                  ))
  (load! (concat "config/" config))
  )
