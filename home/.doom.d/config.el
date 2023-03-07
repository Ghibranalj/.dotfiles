;;; $DOOM_DIR/config.el -*- lexical-binding: t; -*-

(load! "keymap.el")
(setq display-line-numbers-type 'relative)
(setq org-directory "~/org/")

(setq doom-theme 'doom-material-dark)
(setq doom-font (font-spec
                 :family "Source Code Pro"
                 :size 15))

(setq doom-variable-pitch-font (font-spec
                                :family "Source Code Pro"
                                :size 15))

(setq doom-big-font (font-spec
                     :family "Source Code Pro"
                     :size 22))

(setq doom-bin "doom")

;; Auto save
(setq auto-save-default t
      make-backup-files t)

(load! "my-packages/evil-megasave-mode.el")
(add-hook! 'prog-mode-hook 'evil-megasave-mode)
(add-hook! 'git-commit-mode-hook 'evil-megasave-mode)
(add-hook! 'conf-mode-hook 'evil-megasave-mode)
(add-hook! 'yaml-mode-hook 'evil-megasave-mode)

(defvar my-new-frame-hook nil
  "Hook run after a any new frame is created.")

(defvar my-new-gui-frame-hook nil
  "Hook run after a any new gui frame is created.")

(defun on-new-frame ()
  "This is executed when a new frame is created."
  (run-hooks 'my-new-frame-hook)
  (if window-system
      (run-hooks 'my-new-gui-frame-hook)))

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
        centaur-tabs-height 42))


;; tremacs colors
(custom-set-faces!
  '(treemacs-root-face :foreground "#F78C6C")
  '(doom-themes-treemacs-root-face :foreground "#F78C6C"))


;; Coplilot
(defun +copilot/tab ()
  "Copilot completion."
  (interactive)
  (or (copilot-accept-completion)
      (indent-relative)))

(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind (("<backtab>" . 'copilot-accept-completion-by-word)
         ("<backtab>" . 'copilot-accept-completion-by-word)
         :map company-active-map
         ("<tab>" . '+copilot/tab)
         ("TAB" . '+copilot/tab)
         :map company-mode-map
         ("<tab>" . '+copilot/tab)
         ("TAB" . '+copilot/tab)))

;; man pages
(setq Man-notify-method 'pushy)
;; (add-hook! 'Man-mode-hook 'writeroom-mode)
;; (remove-hook! 'writeroom-mode-hook '+zen-enable-text-scaling-mode-h)
;; (remove-hook! 'writeroom-mode-hook '+zen-enable-mixed-pitch-mode-h)

(defun my-comment-or-uncomment()
  "Comment or uncomment the current line or region."
  (interactive)
  (if mark-active
      (comment-or-uncomment-region (region-beginning) (region-end))
    (comment-or-uncomment-region (line-beginning-position) (line-end-position))))

(beacon-mode 1)
(setq scroll-margin 16
      scroll-conservatively 101
      scroll-up-aggressively 0.01
      scroll-down-aggressively 0.01
      scroll-preserve-screen-position t
      auto-window-vscroll t)

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
(add-hook! 'prog-mode-hook #'lsp-headerline-breadcrumb-mode)

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

  (require 'evil)
  (evil-collection-define-key 'normal 'eaf-mode-map*
    "j" 'eaf-send-down-key
    "k" 'eaf-send-up-key
    "h" 'eaf-send-left-key
    "l" 'eaf-send-right-key
    "Q" 'kill-current-buffer
    "R" 'eaf-restart-process
    "B" '(lambda () (interactive) (eaf-execute-app-cmd 'eaf-py-proxy-insert_or_save_as_bookmark))
    )
  )

(add-hook! 'my-new-gui-frame-hook 'my-setup-browser)

(defun my-open-browser(url &optional args)
  "Open URL with ARGS on eaf-browser when not terminal, chrome when terminal."
  (if window-system
      (eaf-open-browser url args)
    (browse-url-chrome url args)))


(setq browse-url-browser-function 'my-open-browser)

(defun my-open-selected-link ()
  "Open URL with ARGS on eaf-browser when not terminal, chrome when terminal."
  (interactive)
  (if (region-active-p)
      (let ((url (buffer-substring-no-properties (region-beginning) (region-end))))
        (my-open-browser url))
    (message "No region selected")))

(defun my-google-search ()
  "Search Google inside eaf-browser."
  (interactive)
  (my-open-browser
   (concat
    "https://www.google.com/search?q="
    (url-hexify-string (if mark-active
                           (buffer-substring (region-beginning) (region-end))
                         (my-read-string-hist "Search Google: " 'my-google-search-history))))))

(defun my-clear-google-search-history ()
  "Clear google search history."
  (interactive)
  (persist-reset 'my-google-search-history)
  (setq my-google-search-history nil))

(defun my-open-github ()
  "Open github in eaf-browser."
  (interactive)
  (require 'browse-at-remote)
  (if (car (browse-at-remote--get-remotes))
      (+vc/browse-at-remote-homepage)
    (my-open-browser "github.com")))

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
                     (eq (buffer-local-value 'major-mode x) 'eaf-mode))
                   (persp-buffer-list))))))

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
                     (or
                      (eq (buffer-local-value 'major-mode x) 'vterm-mode)))
                   (persp-buffer-list))))))

(defvar my-consult--dired-source
  (list :name     "Dired"
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
                     (or
                      (eq (buffer-local-value 'major-mode x) 'dired-mode)))
                   (persp-buffer-list))))))

(defvar my-consult--workspace-source
  (list :name    "Workspace Buffers"
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
                      (not (eq (buffer-local-value 'major-mode x) 'vterm-mode))
                      (not (eq (buffer-local-value 'major-mode x) 'eaf-mode))
                      (not (eq (buffer-local-value 'major-mode x) 'dired-mode))
                      (or (not (boundp 'minimap-buffer-name))
                          (not (string= (buffer-name x) minimap-buffer-name)))))
                   (persp-buffer-list))))))

(after! consult
  (add-to-list 'consult-buffer-sources 'my-consult--eaf-source 'append)
  (add-to-list 'consult-buffer-sources 'my-consult--terminal-source 'append)
  (add-to-list 'consult-buffer-sources 'my-consult--workspace-source 'append)
  (add-to-list 'consult-buffer-sources 'my-consult--dired-source 'append)
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
   '(my-consult--dired-source my-consult--terminal-source my-consult--workspace-source my-consult--eaf-source )
   :require-match
   (confirm-nonexistent-file-or-buffer)
   :prompt (format "Switch to buffer (%s): "
                   (+workspace-current-name))
   :history 'consult--buffer-history
   :sort nil))


(use-package! sidekick
  :hook (sidekick-mode . (lambda () (require 'sidekick-evil)))
  :config
  (setq sidekick-window-hide-footer t)
  (setq sidekick-window-take-focus t))

(add-hook! 'Man-mode-hook 'my-add-buffer-to-project)

(defun remove-scratch-buffer ()
  (if (get-buffer "*scratch*")
      (kill-buffer "*scratch*")))

(if (and (daemonp) (string= (daemonp) "term"))
    (progn
      (add-hook 'after-change-major-mode-hook 'remove-scratch-buffer)
      (add-hook 'server-after-make-frame-hook 'remove-scratch-buffer)))

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
  "Connect to remote ssh in a new workspace."
  (interactive)
  (let (( conn-str (format "%s@%s"
                           (my-read-string-hist "User (ssh): " 'my-ssh-user-history)
                           (my-read-string-hist "Host (ssh): " 'my-ssh-host-history))))
    (progn
      (+workspace-switch conn-str t)
      (dired (format "/scp:%s:" conn-str))
      ;; save
      (persist-save my-ssh-user-history)
      (persist-save my-ssh-host-history)
      )))

(setq projectile-indexing-method 'hybrid)
;; (setq projectile-enable-caching t)

(defun my-save-current-workspace ()
  "Save current workspace."
  (interactive)
  (+workspace/save (+workspace-current-name)))

(put 'dired-find-alternate-file 'disabled nil)
(use-package! dired
  :hook
  (dired-mode . dired-hide-dotfiles-mode)
  (dired-mode . lsp-dired-mode)
  :config
  (setq dired-listing-switches "-agho --group-directories-first"
        dired-dwim-target t
        delete-by-moving-to-trash t
        dired-mouse-drag-files t)
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" '(lambda () (interactive) (find-alternate-file ".."))
    "l" 'dired-find-alternate-file
    ;; "l" 'my-dired-navigate-into
    "." 'dired-hide-dotfiles-mode
    "," 'dired-posframe-show
    "s" 'my-dired-posframe-scroll-down
    "w" 'my-dired-posframe-scroll-up
    ))

(defun dired-count-files-total ()
  (goto-char (point-min))
  (search-forward-regexp dired-move-to-filename-regexp nil t)
  (- (count-lines (line-beginning-position) (point-max)) 2)
  )

(defun my-disable-dotfiles-hide-when-empty ()
  (interactive)
  (let ((dired-files (dired-count-files-total)))
    (if (and (eq dired-files 0)  dired-hide-dotfiles-mode)
        (progn
          (dired-hide-dotfiles-mode -1)
          (message "Showing all dotfiles.")
          )))
  )

(add-hook 'dired-after-readin-hook 'my-disable-dotfiles-hide-when-empty)

(defun my-setup-ivy ()
  (require 'ivy)
  (require 'ivy-posframe)
  (setq ivy-posframe-border-width 2)
  (set-face-attribute 'ivy-posframe-border nil :background "#585858")
  (set-face-attribute 'ivy-posframe nil :background "#212121" :foreground "#EEFFFF")
  (define-key ivy-minibuffer-map (kbd "<escape>") 'minibuffer-keyboard-quit)
  (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-top-center)))
  (setq ivy-posframe-parameters
        '((left-fringe . 8)
          (right-fringe . 8)))
  (ivy-posframe-mode 1))

(add-hook! 'my-new-gui-frame-hook 'my-setup-ivy)

(defun my-read-string-hist (prompt &optional hist)
  "Read a string from the minibuffer with PROMPT. History is stored in HIST."

  (let ((histlen (+ (length (symbol-value hist)) 1)))
    (setq vertico-posframe-height histlen))

  (setq vertico-count-format  (cons "%-0s" ""))

  (let ((result
         (completing-read prompt (symbol-value hist) nil nil nil hist)))
    (if (string= result "")
        nil
      result)))

(defun my-add-buffer-to-project ()
  "Add current buffer to current project."
  (interactive)
  (persp-add-buffer (current-buffer)))


(add-hook! 'daemons-mode-hook 'my-add-buffer-to-project)
(add-hook! 'daemons-output-mode-hook 'my-add-buffer-to-project)
(after! 'daemons
  (evil-collection-define-key 'normal 'daemons-mode-map
    "<ret>" 'daemons-status-at-point
    "s" 'daemons-start-at-point
    "S" 'daemons-stop-at-point
    "r" 'daemons-reload-at-point
    "R" 'daemons-restart-at-point
    "e" 'daemons-enable-at-point
    "d" 'daemons-disable-at-point
    "t" 'daemons-systemd-toggle-user))

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
  '(rainbow-delimiters-depth-6-face :foreground "#144BC3"))

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
    ))

(setq highlight-indent-guides-highlighter-function 'my-highlight-function)

(require 'async-completing-read)
(setq acr-refresh-completion-ui 'consult-vertico--refresh)
(defun my-find-file-in-directory (directory)
  "Finds file in DIRECTORY recursively"
  (interactive `(,default-directory))
  (let*(
        (shell "bash")
        (find (if (executable-find "fd")
                  "fd -tf -tl -c never -H -E .git -I --prune -L"
                "find -type f -printf '%P\n'"))
        (tramp-p (string-match-p "\/scp:" directory))
        (command (if tramp-p
                     find
                   (format "cd %s; %s" directory find)))
        (display-directory (if (and (not tramp-p) (string-match-p (getenv "HOME") directory))
                               (replace-regexp-in-string "/\./$" ""
                                                         (format "~/%s" (file-relative-name directory (getenv "HOME"))))
                             directory))
        (file
         (async-completing-read (format "Find file (%s): " display-directory ) (acr-lines-from-process shell "-c" command)
                                (lambda (x) (not (string-match-p  "\*async-completing-read\*" x))))))
    (when file
      (if tramp-p
          (find-file (expand-file-name file directory))
        (find-file (expand-file-name file))))))

(define-generic-mode 'xmodmap-mode
  '(?!)
  '("add" "clear" "keycode" "keysym" "pointer" "remove")
  nil
  '("[xX]modmap\\(rc\\)?\\'")
  nil
  "Simple mode for xmodmap files.")

(add-hook! 'xmodmap-mode-hook 'display-line-numbers-mode)
(add-hook! 'c-mode-hook (lambda () (c-toggle-comment-style -1)))
(add-hook! magit-post-refresh-hook 'forge-pull)

(defun my-lookup-password (&rest keys)
  (auth-source-forget-all-cached)
  (let ((result (apply #'auth-source-search keys)))
    (if result
        (funcall (plist-get (car result) :secret))
      result)))

(setq smudge-oauth2-callback-port "3725")
(defun my-start-smudge ()
  (interactive)
  (require 'smudge)
  (unless (my-is-service-active-p "spotifyd")
    (my-reload-spotifyd))
  (setq smudge-oauth2-client-secret (my-lookup-password :host "api.spotify.com"))
  (setq smudge-oauth2-client-id (my-lookup-password :host "id.spotify.com"))
  (global-smudge-remote-mode)
  )

(defun my-pause-music-start-again (time)
  (interactive '("2 min"))
  (unless (featurep 'smudge)
    (my-start-smudge))
  (smudge-api-pause)
  (run-at-time time nil #'smudge-api-play)
  )

(defun my-reload-spotifyd ()
  (interactive)
  (shell-command "systemctl reload-or-restart --user spotifyd"))

(defun my-smudge-set-volume (volume)
  (interactive `(,(read-string "Volume: " nil)))

  (if (stringp volume)
      (setq volume (string-to-number volume)))

  (when (and  smudge-selected-device-id (<= volume 100) (>= volume 0) )
    (smudge-api-set-volume smudge-selected-device-id volume))
  )

(defun my-is-service-active-p (service &optional root)
  "Return t if SERVICE is active. use --user if ROOT is nil"
  (let ((active (s-trim
                 (shell-command-to-string
                  (format "systemctl is-active %s %s" (if root "" "--user") service)))))
    (string= active "active")))

(defun my-open-man (page)
  (interactive `(,(read-string "Man: " nil)))
  (man page))


(after! web-mode
  (defun +web/indent-or-yas-or-emmet-expand ()
    "Just run (+copilot/tab)"
    (interactive)
    (+copilot/tab)
    ))


(defun my-setup-tcp-server ()
  (interactive)
  (setq   server-use-tcp t
          server-host "127.0.0.1"
          server-port 6666
          server-auth-dir "~/.emacs.d/server"
          server-auth-key "1234"
          )
  )

(defun my-chmod-this-file ( mode )
  (interactive `(,(read-string "File Mode: " nil)))
  (shell-command (format "chmod %s %s" mode (buffer-file-name)))
  )

(add-hook! 'lsp-mode-hook '(lambda ()
                             (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\tmp\\'")
                             ))

(add-hook! 'projectile-mode-hook '(lambda ()
                                    (add-to-list 'projectile-globally-ignored-directories "tmp")
                                    (add-to-list 'projectile-globally-ignored-directories "vendor")
                                    (add-to-list 'projectile-globally-ignored-directories "CMakeFiles")
                                    (add-to-list 'projectile-globally-ignored-directories "build")
                                    ))

(defun my-dap-debug-last()
  (interactive)
  (call-interactively '+make/run-last)
  (call-interactively 'dap-debug-last)
  ;; (call-interactively 'dap-hydra)
  )

(defun my-dap-debug ()
  (interactive)
  (call-interactively '+make/run)
  (call-interactively 'dap-debug)
  ;; (call-interactively 'dap-hydra)
  )

(use-package! dap-mode
  :custom
  ;; (sessions locals breakpoints expressions controls tooltip)
  (dap-auto-configure-features '(locals controls tooltip))
  )

(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-language-id-configuration
               '(glsl-mode . "glsl"))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '("glslls" "--stdin"))
                    :activation-fn (lsp-activate-on "glsl")
                    :server-id 'glslls)))

(use-package blamer
  :bind (("s-i" . blamer-show-commit-info))
  :defer 20
  :custom
  (blamer-idle-time 1)
  (blamer-min-offset 80)
  (blamer-author-formatter "  ✎ %s ")
  (blamer-datetime-formatter "[%s] ")
  :custom-face
  (blamer-face ((t :foreground "#805d96"
                   :background nil
                   :height 115
                   :italic t)))
  :config
  (global-blamer-mode 1))


(defun my-poshandler (info)
  ;; (cons x y)
  (cons
   (/ (- (plist-get info :parent-frame-width) (plist-get info :posframe-width)) 2) ;x
   25 ; y
   )
  )

(use-package! vertico-posframe
  :after vertico
  :config
  (setq vertico-posframe-parameters
        '((left-fringe . 10)
          (right-fringe . 10)
          ))
  (setq vertico-posframe-width 110)
  (setq vertico-posframe-poshandler 'my-poshandler)
  )
(add-hook! 'my-new-gui-frame-hook 'vertico-posframe-mode)

(use-package! which-key-posframe
  :config
  (setq which-key-posframe-poshandler 'my-poshandler)
  (setq which-key-posframe-parameters
        '((left-fringe . 20)
          (right-fringe . 20))))

(add-hook! 'my-new-gui-frame-hook 'which-key-posframe-mode)

(defun my-load-read-string()
  (load! "read-string.el" doom-user-dir)
  )

(add-hook! 'my-new-gui-frame-hook 'my-load-read-string)

(defun my-find-major-mode-for-file (filename)
  "Find the major mode associated with the given file name."
  (let ((alist auto-mode-alist)
        (mode nil))
    (while (and alist (not mode))
      (if (string-match (caar alist) filename)
          (setq mode (cdar alist))
        (setq alist (cdr alist))))
    mode))

(defun my-dired-posframe-highlight()
  (let* ((file (dired-get-filename nil t))
         (themode (my-find-major-mode-for-file (file-name-nondirectory file)))
         )
    (if (file-directory-p file)
        (setq themode 'dired-mode))
    (with-current-buffer (get-buffer  dired-posframe-buffer)
      (if (not (eq themode nil))
          (progn
            (funcall themode)
            ( goto-char (point-min))
            (read-only-mode -1)
            (insert (format "%s %s %s\n" comment-start (file-name-nondirectory file) comment-end))
            )
        )
      )
    )
  )

(after! dired-posframe
  :config
  (setq dired-posframe-width 65)
  (setq dired-posframe-height 25)
  (setq dired-posframe-min-height nil)
  (setq dired-posframe-min-width nil)
  (setq dired-posframe-parameters
        '((left-fringe . 10)
          (right-fringe . 10)
          ))
  (advice-add 'dired-posframe--show :after 'my-dired-posframe-highlight)
  (advice-add 'keyboard-quit :before 'posframe-delete-all)
  (fset 'dired-posframe--hide 'ignore)
  )

(defun my-dired-posframe-scroll-down()
  (interactive)
  (with-current-buffer (get-buffer  dired-posframe-buffer)
    (read-only-mode -1)
    (goto-char (point-min))
    ;; copy first line
    (let ((line (buffer-substring-no-properties (point) (line-end-position))))
      (delete-region (point) (line-end-position))
      (delete-char 1)
      (goto-char (point-max))
      (insert (format "%s\n" line))
      )
    )
  )

(defun my-dired-posframe-scroll-up()
  (interactive)
  (with-current-buffer (get-buffer  dired-posframe-buffer)
    (read-only-mode -1)
    (goto-char (point-max))
    ;; copy last line
    (let ((line (buffer-substring-no-properties (line-beginning-position) (point))))
      (delete-region (line-beginning-position) (point))
      (delete-char -1)
      (goto-char (point-min))
      (insert (format "%s\n" line))
      )
    )
  )

(defun my-dired-navigate-into ()
  "Open directory in same dired buffer. Open file in new buffer"
  (interactive)
  (let (
        (file (dired-get-filename nil t)))
    (if (file-directory-p file)
        (dired-find-alternate-file)
      (dired-find-file)
      )
    )
  )
