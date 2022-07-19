;;; ../.dotfiles/doom/.doom.d/config.el -*- lexical-binding: t; -*-
;;;

(defun +my/custom-ascii ()
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
(setq +doom-dashboard-ascii-banner-fn #'+my/custom-ascii)

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
                     :size 24
                     ))

;; Auto save
(setq auto-save-default t
      make-backup-files t)
(super-save-mode +1)
(setq super-save-auto-save-when-idle t)

(defvar +my/new-frame-hook nil
  "Hook run after a any new frame is created.")

(defvar +my/new-gui-frame-hook nil
  "Hook run after a any new gui frame is created.")

(defun on-new-frame ()
  "This is executed when a new frame is created."
  (run-hooks '+my/new-frame-hook)
  (if window-system
      (run-hooks '+my/new-gui-frame-hook))

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

(after! company
  (setq company-show-quick-access t)
  (setq company-idle-delay 0)
  )

(after! magit
  (setq magit-diff-refine-hunk 'all)
  )

;; Minimap

(after! minimap
  (setq
   ;; Configure minimap position
   minimap-window-location 'right ; Minimap on the right side
   minimap-width-fraction 0.0 ; slightly smaller minimap
   minimap-minimum-width 10 ; also slightly smaller minimap
   minimap-enlarge-certain-faces nil ; enlarge breaks BlockFont
   )
  )
(custom-set-faces!
  '(minimap-font-face :height 12 :group 'minimap))
;; (minimap-mode 1)


;; man pages
(setq Man-notify-method 'pushy)

(defun +my/comment-or-uncomment()
  "Comment or uncomment the current line or region."
  (interactive)
  (if mark-active
      (comment-or-uncomment-region (region-beginning) (region-end))
    (comment-or-uncomment-region (line-beginning-position) (line-end-position))))

(beacon-mode 1)

(setq scroll-margin 10)

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

(defun +my/eval-line ()
  "Evaluate the current line."
  (interactive)
  (eval-region (line-beginning-position) (line-end-position)))

(require 'persist)
(add-hook! 'after-delete-frame-functions #'(lambda (frame) (persist--save-all)))

;; eaf and browser
(defun +my/setup-browser ()
  "Setup eaf and browser."
  (message "Browser is being setup")
  (add-to-list 'load-path "~/.emacs.d/site-lisp/emacs-application-framework/")
  (require 'eaf)
  (require 'eaf-browser)
  (require 'eaf-demo)
  (eaf-setq eaf-browser-enable-bookmark "true")
  (eaf-setq eaf-browser-enable-adblocker "true")
  (defvar eaf-browser-default-search-engine "google")
  (add-hook! 'eaf-mode-hook '(lambda () (persp-add-buffer (current-buffer))))
  (add-hook! 'eaf-mode-hook 'hide-mode-line-mode)

  (evil-collection-define-key 'normal 'eaf-mode-map*
    "j" 'eaf-send-down-key
    "k" 'eaf-send-up-key
    "h" 'eaf-send-left-key
    "l" 'eaf-send-right-key
    "Q" 'kill-current-buffer
    )
  )
(add-hook! '+my/new-gui-frame-hook '+my/setup-browser)


(defun +my/open-browser(url &optional args)
  "Open URL with ARGS on eaf-browser when not terminal, chrome when terminal."
  (if window-system
      (eaf-open-browser url args)
    (browse-url-chrome url args)
    ))

(setq browse-url-browser-function '+my/open-browser)

;; TODO save this across emacs session
(persist-defvar +my/google-search-history nil
                "History for google search.")

(defun +my/google-search ()
  "Search Google inside eaf-browser."
  (interactive)
  (+my/open-browser
   (concat
    "https://www.google.com/search?q="
    (url-hexify-string (if mark-active
                           (buffer-substring (region-beginning) (region-end))
                         (+my/read-string "Search Google: " '+my/google-search-history))))))

(defun +my/clear-google-search-history ()
  "Clear google search history."
  (interactive)
  (persist-reset '+my/google-search-history)
  (setq +my/google-search-history nil)
  )

(defun +my/open-github ()
  "Open github in eaf-browser."
  (interactive)
  (if (not (eq (magit-git-dir (projectile-project-root)) nil))
      (+vc/browse-at-remote-homepage)
    (+my/open-browser "github.com")
    ))


(defvar +my/consult--eaf-source
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


(defvar +my/consult--terminal-source
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

(defvar +my/consult--workspace-source
  (list :name    "Buffers"
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
                      )
                     )
                   (buffer-list))))))

(after! consult
  (add-to-list 'consult-buffer-sources '+my/consult--eaf-source 'append)
  (add-to-list 'consult-buffer-sources '+my/consult--terminal-source 'append)
  (add-to-list 'consult-buffer-sources '+my/consult--workspace-source 'append)
  )

(require 'consult)
(defun +my/consult-browser ()
  "Open eaf-browser."
  (interactive)
  (consult-buffer '(+my/consult--eaf-source)))

(defun +my/consult-terminal ()
  "Open terminal."
  (interactive)
  (consult-buffer '(+my/consult--terminal-source)))

(defun +my/consult-workspace ()
  "Switch to buffer in workspace.
Shows terminal in seperate section. Also shows browsers."
  (interactive)
  (consult--multi
   '(+my/consult--workspace-source +my/consult--terminal-source +my/consult--eaf-source)
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
  ;; uncomment to enable vscode-like command palette (note doesn't work with eaf)
  ;; (vertico-posframe-mode 1
  (setq vertico-posframe-poshandler 'posframe-poshandler-frame-top-center)
  )

(add-hook! '+my/new-gui-frame-hook 'vertico-posframe-mode)

(use-package! sidekick
  :hook (sidekick-mode . (lambda () (require 'sidekick-evil)))
  :config
  (setq sidekick-window-hide-footer t)
  (setq sidekick-window-take-focus t)
  )
(add-hook! 'Man-mode-hook '(lambda () (persp-add-buffer (current-buffer))))

(defun remove-scratch-buffer ()
  (if (get-buffer "*scratch*")
      (kill-buffer "*scratch*")))

(if (and (daemonp) (string= (daemonp) "term"))
    (progn
      (add-hook 'after-change-major-mode-hook 'remove-scratch-buffer)
      (add-hook 'server-after-make-frame-hook 'remove-scratch-buffer)
      )
  )

(persist-defvar +my/ssh-user-history nil
                "History for ssh user.")
(persist-defvar +my/ssh-host-history nil
                "History for ssh host.")

(defun +my/connect-remote-ssh()
  (interactive)
  (dired (format "/scp:%s@%s:"
                 (+my/read-string "User (ssh): " '+my/ssh-user-history)
                 (+my/read-string "Host (ssh): " '+my/ssh-host-history))))

(setq projectile-indexing-method 'native)
(setq projectile-enable-caching t)

(defun +my/save-current-workspace ()
  "Save current workspace."
  (interactive)
  (+workspace/save (+workspace-current-name)))

(setq dired-listing-switches "-agho --group-directories-first")
(evil-collection-define-key 'normal 'dired-mode-map
  "h" 'dired-up-directory
  "l" 'dired-find-file
  ;; "<right>" 'dired-up-directory
  ;; "<left>" 'dired-find-file
  )
(load! "keymap.el")

(defun +my/setup-ivy ()
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

(add-hook! '+my/new-gui-frame-hook '+my/setup-ivy)

(defun +my/read-string (prompt &optional hist)
  "Read a string from the minibuffer with PROMPT. History is stored in HIST."
  (let ((string (ivy-read prompt (symbol-value hist) :history hist :require-match nil)))
    (if (string= string "")
        nil
      string)))
