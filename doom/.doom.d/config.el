;;; ../.dotfiles/doom/.doom.d/config.el -*- lexical-binding: t; -*-

(load! "keymap.el")
(defun my-custom-ascii ()
  "To display my ascii art to doom splash."
  (mapc (lambda (line)
          (insert (propertize (+doom-dashboard--center +doom-dashboard--width line)
                              'face 'doom-dashboard-banner) " ")
          (insert "\n"))
        (split-string  "
   ▄██████▄     ▄█    █▄     ▄█  ▀█████████▄  ▀█████████▄  ▄██   ▄
  ███    ███   ███    ███   ███    ███    ███   ███    ███ ███   ██▄
  ███    █▀    ███    ███   ███▌   ███    ███   ███    ███ ███▄▄▄███
 ▄███         ▄███▄▄▄▄███▄▄ ███▌  ▄███▄▄▄██▀   ▄███▄▄▄██▀  ▀▀▀▀▀▀███
▀▀███ ████▄  ▀▀███▀▀▀▀███▀  ███▌ ▀▀███▀▀▀██▄  ▀▀███▀▀▀██▄  ▄██   ███
  ███    ███   ███    ███   ███    ███    ██▄   ███    ██▄ ███   ███
  ███    ███   ███    ███   ███    ███    ███   ███    ███ ███   ███
  ████████▀    ███    █▀    █▀   ▄█████████▀  ▄█████████▀   ▀█████▀
--
Doom Emacs" "\n" t)))
(setq +doom-dashboard-ascii-banner-fn #'my-custom-ascii)

(setq display-line-numbers-type 'relative)

(setq org-directory "~/org/")

(setq doom-theme 'doom-material-dark)
(setq doom-font (font-spec
                 :family "Source Code Pro"
                 :size 15
                 ))
(setq doom-variable-pitch-font (font-spec
                                :family "Source Code Pro"
                                :size 14
                                ))
(setq doom-big-font (font-spec
                     :family "Source Code Pro"
                     :size 20
                     ))

;; Auto save
(setq auto-save-default t
      make-backup-files t)

(defun my-save-when-has-file ()
  (when (buffer-file-name)
    (save-buffer)))

(defun my-save-unless-insert (&rest _)
  (unless (evil-insert-state-p)
    (my-save-when-has-file)))

(add-hook! 'after-change-functions 'my-save-unless-insert)
(add-hook! 'evil-insert-state-exit-hook 'my-save-when-has-file)


(defvar my-new-frame-hook nil
  "Hook run after a any new frame is created.")

(defvar my-new-gui-frame-hook nil
  "Hook run after a any new gui frame is created.")

(defun on-new-frame ()
  "This is executed when a new frame is created."
  (run-hooks 'my-new-frame-hook)
  (if window-system
      (run-hooks 'my-new-gui-frame-hook))
  )

;; Running on daemon startup
(if (daemonp)
    (add-hook 'after-make-frame-functions (lambda (frame)
                                            (with-selected-frame frame
                                              (on-new-frame))))
  (on-new-frame))

;; Scroll bar
(global-yascroll-bar-mode 1)

;; Tabs
(after! centaur-tabs
  (setq centaur-tabs-set-bar 'under
        centaur-tabs-set-close-button nil
        centaur-tabs-height 42
        ))

;; tremacs colors
(custom-set-faces!
  '(treemacs-root-face :foreground "#F78C6C" )
  '(doom-themes-treemacs-root-face :foreground "#F78C6C" )
  )

;; Coplilot
(defun +copilot/tab ()
  "Copilot completion."
  (interactive)
  (or (copilot-accept-completion)
      (indent-relative)))
;; (minimap-mode 1)


;; man pages
(setq Man-notify-method 'pushy)

(defun my-comment-or-uncomment()
  "Comment or uncomment the current line or region."
  (interactive)
  (if mark-active
      (comment-or-uncomment-region (region-beginning) (region-end))
    (comment-or-uncomment-region (line-beginning-position) (line-end-position))))

(beacon-mode 1)

(setq scroll-margin 16)

;; misc hook
(use-package! company-box
  :hook (company-mode . company-box-mode))

(setq lsp-ui-sideline-show-diagnostics t)
(setq lsp-ui-sideline-show-hover t)
(setq lsp-ui-sideline-show-code-actions t)
(setq lsp-ui-sideline-update-mode 'line)
(add-hook! 'prog-mode-hook #'lsp-ui-mode)
(add-hook! 'lsp-mode-hook #'lsp-ui-sideline-mode)
(add-hook! 'lsp-mode-hook #'lsp-headerline-breadcrumb-mode)
(setq lsp-headerline-breadcrumb-enable t)

(setq magit-clone-default-directory "~/Workspace/")

(defun my-eval-line ()
  "Evaluate the current line."
  (interactive)
  (eval-region (line-beginning-position) (line-end-position)))


;; eaf and browser
(defun my-setup-browser ()
  "Setup eaf and browser."
  (message "Browser is being setup")
  (add-to-list 'load-path "~/.emacs.d/site-lisp/emacs-application-framework/")
  (require 'eaf)
  (require 'eaf-browser)
  (require 'eaf-video-player)
  (require 'eaf-pdf-viewer)
  ;; (setq eaf-browser-enable-bookmark t)
  (setq eaf-browser-enable-adblocker t)
  (defvar eaf-browser-default-search-engine "google")
  (add-hook! 'eaf-mode-hook 'my-add-buffer-to-project)
  (add-hook! 'eaf-mode-hook 'hide-mode-line-mode)

  (evil-collection-define-key 'normal 'eaf-mode-map*
    "j" 'eaf-send-down-key
    "k" 'eaf-send-up-key
    ;; "h" 'eaf-send-left-key
    ;; "l" 'eaf-send-right-key
    "Q" 'kill-current-buffer
    "R" 'eaf-restart-process
    )
  )
(add-hook! 'my-new-gui-frame-hook 'my-setup-browser)

(defun my-open-browser(url &optional args)
  "Open URL with ARGS on eaf-browser when not terminal, chrome when terminal."
  (if window-system
      (eaf-open-browser url args)
    (browse-url-chrome url args)
    ))

(setq browse-url-browser-function 'my-open-browser)


(defun my-google-search ()
  "Search Google inside eaf-browser."
  (interactive)
  (my-open-browser
   (concat
    "https://www.google.com/search?q="
    (url-hexify-string (if mark-active
                           (buffer-substring (region-beginning) (region-end))
                         (my-read-string "Search Google: " 'my-google-search-history))))))

(defun my-clear-google-search-history ()
  "Clear google search history."
  (interactive)
  (persist-reset 'my-google-search-history)
  (setq my-google-search-history nil)
  )
(defun my-open-github ()
  "Open github in eaf-browser."
  (interactive)
  (require 'browse-at-remote)
  (if (car (browse-at-remote--get-remotes))
      (+vc/browse-at-remote-homepage)
    (my-open-browser "github.com")

    ))

;; TODO diferenciate between video player and browser maybe?
(defvar my-consult--eaf-source
  (list :name     "Browser"
        :category 'buffer
        :narrow   ?o
        :face     'consult-buffer
        :history  'buffer-name-history
        :state    #'consult--buffer-state
        :require-match t
        :items
        (lambda ()
          (mapcar #'buffer-name
                  (seq-filter
                   (lambda (x)
                     (and (eq (buffer-local-value 'major-mode x) 'eaf-mode)
                          (+workspace-contains-buffer-p x)
                          ))
                   (buffer-list))))))



(defvar my-consult--terminal-source
  (list :name     "Terminal"
        :category 'buffer
        :narrow   ?o
        :face     'consult-buffer
        :history  'buffer-name-history
        :state    #'consult--buffer-state
        :require-match t
        :items
        (lambda ()
          (mapcar #'buffer-name
                  (seq-filter
                   (lambda (x)
                     (and
                      (eq (buffer-local-value 'major-mode x) 'vterm-mode)
                      (+workspace-contains-buffer-p x)
                      )
                     )
                   (buffer-list))))))

(defvar my-consult--workspace-source
  (list :name    "Workspace Buffer"
        :category 'buffer
        :narrow   ?o
        :face     'consult-buffer
        :history  'buffer-name-history
        :state    #'consult--buffer-state
        :require-match t
        :items
        (lambda ()
          (mapcar #'buffer-name
                  (seq-filter
                   (lambda (x)
                     (and
                      (+workspace-contains-buffer-p x)
                      (not (eq (buffer-local-value 'major-mode x) 'vterm-mode))
                      (not (eq (buffer-local-value 'major-mode x) 'eaf-mode))
                      (or (not (boundp 'minimap-buffer-name))
                          (not (string= (buffer-name x) minimap-buffer-name )))
                      )
                     )
                   (buffer-list))))))



(after! consult
  (add-to-list 'consult-buffer-sources 'my-consult--eaf-source 'append)
  (add-to-list 'consult-buffer-sources 'my-consult--terminal-source 'append)
  (add-to-list 'consult-buffer-sources 'my-consult--workspace-source 'append)
  )

(require 'consult)
(defun my-consult-browser ()
  "Open eaf-browser."
  (interactive)
  (consult-buffer '(my-consult--eaf-source)))

(defun my-consult-terminal ()
  "Open terminal."
  (interactive)
  (consult-buffer '(my-consult--terminal-source)))

(defun my-consult-workspace ()
  "Switch to buffer in workspace.
Shows terminal in seperate section. Also shows browsers."
  (interactive)
  (consult--multi
   '(my-consult--workspace-source my-consult--terminal-source my-consult--eaf-source)
   :require-match
   (confirm-nonexistent-file-or-buffer)
   :prompt (format "Switch to buffer (%s): "
                   (+workspace-current-name))
   :history 'consult--buffer-history
   :sort nil)
  )

(after! vertico-posframe
  (setq vertico-posframe-parameters
        '((left-fringe . 8)
          (right-fringe . 8)))
  (setq vertico-posframe-poshandler 'posframe-poshandler-frame-top-center)
  )

(add-hook! 'my-new-gui-frame-hook 'vertico-posframe-mode)

(use-package! sidekick
  :hook (sidekick-mode . (lambda () (require 'sidekick-evil)))
  :config
  (setq sidekick-window-hide-footer t)
  (setq sidekick-window-take-focus t)
  )
(add-hook! 'Man-mode-hook 'my-add-buffer-to-project)

(defun remove-scratch-buffer ()
  (if (get-buffer "*scratch*")
      (kill-buffer "*scratch*")))

(if (and (daemonp) (string= (daemonp) "term"))
    (progn
      (add-hook 'after-change-major-mode-hook 'remove-scratch-buffer)
      (add-hook 'server-after-make-frame-hook 'remove-scratch-buffer)
      )
  )

(require 'persist)
(persist-defvar my-ssh-user-history nil
                "History for ssh user.")

(persist-defvar my-ssh-host-history nil
                "History for ssh host.")

(persist-defvar my-google-search-history nil
                "History for google search.")
(add-hook! 'my-new-frame-hook
           '(lambda ()
              (dolist (x persist--symbols)
                (persist-load x))))

(add-hook! 'after-delete-frame-functions '(lambda (frame) (persist--save-all)))

(defun my-connect-remote-ssh()
  (interactive)
  (dired (format "/scp:%s@%s:"
                 (my-read-string "User (ssh): " 'my-ssh-user-history)
                 (my-read-string "Host (ssh): " 'my-ssh-host-history))))

(setq projectile-indexing-method 'native)
;; (setq projectile-enable-caching t)

(defun my-save-current-workspace ()
  "Save current workspace."
  (interactive)
  (+workspace/save (+workspace-current-name)))

(use-package! dired
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (setq dired-listing-switches "-agho --group-directories-first")
  (setq dired-dwim-target t)
  (setq delete-by-moving-to-trash t)

  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-up-directory
    "l" 'dired-find-file
    "." 'dired-hide-dotfiles-mode
    )
  )

(defun my-setup-ivy ()
  (require 'ivy)
  (require 'ivy-posframe)
  (ivy-posframe-mode 1)
  (setq ivy-posframe-border-width 2)
  (set-face-attribute 'ivy-posframe-border nil :background "#585858")
  (set-face-attribute 'ivy-posframe nil :background "#212121" :foreground "#EEFFFF")
  (define-key ivy-minibuffer-map (kbd "<escape>") 'minibuffer-keyboard-quit)
  (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-top-center)))
  (setq ivy-posframe-parameters
        '((left-fringe . 8)
          (right-fringe . 8)))
  )

(add-hook! 'my-new-gui-frame-hook 'my-setup-ivy)

(defun my-read-string (prompt &optional hist)
  "Read a string from the minibuffer with PROMPT. History is stored in HIST."
  (let ((string (ivy-read prompt (symbol-value hist) :history hist :require-match nil)))
    (if (string= string "")
        nil
      string)))

(defun my-add-buffer-to-project ()
  "Add current buffer to current project."
  (interactive)
  (persp-add-buffer (current-buffer))
  )

(add-hook! 'daemons-mode-hook 'my-add-buffer-to-project)
(add-hook! 'daemons-output-mode-hook 'my-add-buffer-to-project)

(evil-collection-define-key 'normal 'daemons-mode-map
  "<ret>" 'daemons-status-at-point
  "s" 'daemons-start-at-point
  "S" 'daemons-stop-at-point
  "r" 'daemons-reload-at-point
  "R" 'daemons-restart-at-point
  "e" 'daemons-enable-at-point
  "d" 'daemons-disable-at-point
  "t" 'daemons-systemd-toggle-user
  )

(setq vterm-always-compile-module t)

(defun my-delete-other-workspace ()
  (interactive)
  (dolist (workspace (+workspace-list-names))
    (unless (eq workspace (+workspace-current-name))
      (+workspace/delete workspace))))

(setq rainbow-delimiters-max-face-count 6)
(custom-set-faces!
  '(rainbow-delimiters-depth-1-face :foreground "#B23F77")
  '(rainbow-delimiters-depth-2-face :foreground "#B69457")
  '(rainbow-delimiters-depth-3-face :foreground "#7BA55D")
  '(rainbow-delimiters-depth-4-face :foreground "#4F86A0")
  '(rainbow-delimiters-depth-5-face :foreground "#B55C2B")
  '(rainbow-delimiters-depth-6-face :foreground "#144BC3")
  )
(add-hook! 'prog-mode-hook 'rainbow-delimiters-mode)

(defun my-highlight-function (level responsive display)
  "Highlight the current line with a face according to LEVEL.
RESPONSIVE and DISPLAY are ignored."

  (let* ((lvl (% level 6)))
    (cond
     ((eq lvl 0) 'rainbow-delimiters-depth-1-face)
     ((eq lvl 1) 'rainbow-delimiters-depth-2-face)
     ((eq lvl 2) 'rainbow-delimiters-depth-3-face)
     ((eq lvl 3) 'rainbow-delimiters-depth-4-face)
     ((eq lvl 4) 'rainbow-delimiters-depth-5-face)
     ((eq lvl 5) 'rainbow-delimiters-depth-6-face)
     )
    )
  )

(setq highlight-indent-guides-highlighter-function 'my-highlight-function)


;; (defun icomplete-exhibit ()
;;   nil)
(require 'async-completing-read)
(setq acr-refresh-completion-ui 'consult-vertico--refresh)
(defun my-find-file-in-directory (directory)
  "Finds file in DIRECTORY recursively"
  (interactive "P")
  (let*(
        (shell "bash")
        (dir (or directory default-directory))
        (relative (file-relative-name dir (getenv "HOME")))
        (tramp-p (string-match-p "\/scp:" dir))
        (command (if tramp-p
                     "find . -type f"
                   (format "cd %s; find . -type f" dir)))
        (file
         ;; TODO Change this to ivy-read when using ivy
         (async-completing-read (format "Find file (~/%s): " relative) (acr-lines-from-process shell "-c" command))))
        (when (and file (not (string-match-p  "\*async-completing-read\*" file)))
          (if tramp-p
              (find-file (expand-file-name file dir))
            (find-file file)))))
