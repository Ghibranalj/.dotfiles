;;; ../.dotfiles/doom/.doom.d/config/eaf.el -*- lexical-binding: t; -*-

;; eaf and browser

(setq browse-url-browser-function '+my/open-browser)

(defun +my/setup-browser ()
  "Setup eaf and browser."
  (message "Browser is being setup")
  (add-to-list 'load-path "~/.emacs.d/site-lisp/emacs-application-framework/")
  (require 'eaf)
  (require 'eaf-browser)
  (eaf-setq eaf-browser-enable-bookmark "true")
  (eaf-setq eaf-browser-enable-adblocker "true")
  (defvar eaf-browser-default-search-engine "google")
  (add-hook! 'eaf-mode-hook '(lambda () (persp-add-buffer (current-buffer))))
  (add-hook! 'eaf-mode-hook 'hide-mode-line-mode)
  )


(defun +my/load-eaf-on-gui ()
  "Load eaf on GUI."
  (when (window-system)
    (+my/setup-browser)))
(add-hook! '+my/new-frame-hook '+my/load-eaf-on-gui)

(defun +my/open-browser(url &optional args)
  "Open URL with ARGS on eaf-browser when not terminal, chrome when terminal."
  (if window-system
      (eaf-open-browser url args)
    (browse-url-chrome url args)
    ))

(defun +my/google-search ()
  "Search Google inside eaf-browser."
  (interactive)
  (+my/open-browser
   (concat
    "https://www.google.com/search?q="
    (url-hexify-string (if mark-active
                           (buffer-substring (region-beginning) (region-end))
                         (read-string "Search Google for: "))))))

(defun +my/open-github ()
  "Open github in eaf-browser."
  (interactive)
  (if (not (eq (magit-git-dir (projectile-project-root)) nil))
      (+vc/browse-at-remote-homepage)
    (+my/open-browser "github.com")
    ))
