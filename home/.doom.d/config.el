;;; $DOOM_DIR/config.el -*- lexical-binding: t; -*-
(load! "keymap.el" doom-user-dir)
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

;; General Variables
(setq
 display-line-numbers-type 'relative
 org-directory "~/org/"
 scroll-margin 16
 scroll-conservatively 101
 scroll-up-aggressively 0.01
 scroll-down-aggressively 0.01
 scroll-preserve-screen-position t
 auto-window-vscroll t
 enable-local-variables :safe

 ;; Auto save
 auto-save-default t
 make-backup-files t
 )

;; Doom Variables
(setq
 doom-theme 'doom-material-dark
 doom-font (font-spec
            :family "Source Code Pro"
            :size 14)
 doom-variable-pitch-font (font-spec
                           :family "Source Code Pro"
                           :size 14)
 doom-big-font (font-spec
                :family "Source Code Pro"
                :size 20)
 doom-bin "doom"
 )

;; man pages
(setq Man-notify-method 'pushy)
(add-hook! 'Man-mode-hook 'my-add-buffer-to-project)

(use-package! man
  :custom
  (Man-notify-method 'pushy)
  :hook
  (Man-mode 'my-add-buffer-to-project))

(load! "my-packages/evil-megasave.el")
(use-package! evil-megasave
  :hook
  (prog-mode . evil-megasave-mode)
  (git-commit-mode . evil-megasave-mode)
  (conf-mode . evil-megasave-mode)
  (yaml-mode . evil-megasave-mode))

;; tremacs colors
(custom-set-faces!
  '(treemacs-root-face :foreground "#F78C6C")
  '(doom-themes-treemacs-root-face :foreground "#F78C6C"))

;; Coplilot
(defun +copilot/tab ()
  "Copilot completion."
  (interactive)
  (or
   (if (bound-and-true-p emmet-mode)
       (emmet-expand-line nil))
   (copilot-accept-completion)
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


(defun my-comment-or-uncomment()
  "Comment or uncomment the current line or region."
  (interactive)
  (if mark-active
      (comment-or-uncomment-region (region-beginning) (region-end))
    (comment-or-uncomment-region (line-beginning-position) (line-end-position))))

(use-package! company-box
  :hook (company-mode . company-box-mode))

(use-package! lsp-ui-sideline
  :custom
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-sideline-show-diagnostics t)
  (lsp-ui-sideline-show-code-actions t)
  (lsp-ui-sideline-show-symbol t)
  :hook (lsp-mode . lsp-ui-sideline-mode))

(use-package! lsp-ui
  :hook (prog-mode . lsp-ui-mode))

(use-package! lsp-mode
  :custom
  (lsp-headerline-breadcrumb-mode t)
  :hook
  (prog-mode . lsp-headerline-breadcrumb-mode))

(use-package! magit
  :custom
  (magit-clone-default-directory "~/Workspace/")
  :hook
  ;; (magit-post-refresh . forge-pull)
  )

(defun my-eval-line ()
  "Evaluate the current line."
  (interactive)
  (eval-region (line-beginning-position) (line-end-position)))

(use-package! consult
  :config
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
                        (not (eq (buffer-local-value 'major-mode x) 'dired-mode))
                        (or (not (boundp 'minimap-buffer-name))
                            (not (string= (buffer-name x) minimap-buffer-name)))))
                     (persp-buffer-list))))))

  (add-to-list 'consult-buffer-sources 'my-consult--terminal-source 'append)
  (add-to-list 'consult-buffer-sources 'my-consult--workspace-source 'append)
  (add-to-list 'consult-buffer-sources 'my-consult--dired-source 'append)
  )

(defun my-consult-terminal ()
  "Open terminal."
  (interactive)
  (consult-buffer '(my-consult--terminal-source)))

(defun my-consult-workspace ()
  "Switch to buffer in workspace.
Shows terminal in seperate section. Also shows browsers."
  (interactive)
  (consult--multi
   '(my-consult--dired-source my-consult--terminal-source my-consult--workspace-source)
   :require-match
   (confirm-nonexistent-file-or-buffer)
   :prompt (format "Switch to buffer (%s): "
                   (+workspace-current-name))
   :history 'consult--buffer-history
   :sort nil))

(require 'persist)
(persist-defvar my-ssh-user-history nil
                "History for ssh user.")

(persist-defvar my-ssh-host-history nil
                "History for ssh host.")

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

(use-package! dired
  :hook
  (dired-mode . dired-hide-dotfiles-mode)
  ;; (dired-mode . lsp-dired-mode)
  :custom
  (dired-listing-switches "-agho --group-directories-first")
  (dired-dwim-target t)
  (delete-by-moving-to-trash t)
  (dired-mouse-drag-files t)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" '(lambda () (interactive) (find-alternate-file ".."))
    ;; "l" 'dired-find-alternate-file
    "l" 'my-dired-navigate-into
    "." 'dired-hide-dotfiles-mode
    "," 'dired-posframe-show
    "s" 'my-dired-posframe-scroll-down
    "w" 'my-dired-posframe-scroll-up
    "e" 'lsp-dired-mode
    )
  (put 'dired-find-alternate-file 'disabled nil))

(use-package! ivy-posframe
  :custom
  (ivy-posframe-border-width 2)
  (ivy-posframe-width 10)
  (ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-top-center)))
  (ivy-posframe-parameters
   '((left-fringe . 8)
     (right-fringe . 8)))
  :config
  (define-key ivy-minibuffer-map (kbd "<escape>") 'minibuffer-keyboard-quit)
  (set-face-attribute 'ivy-posframe-border nil :background "#585858")
  (set-face-attribute 'ivy-posframe nil :background "#212121" :foreground "#EEFFFF")
  :hook
  (my-new-gui-frame . ivy-posframe-mode))

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

(use-package! daemons
  :config
  (evil-collection-define-key 'normal 'daemons-mode-map
    "<ret>" 'daemons-status-at-point
    "s" 'daemons-start-at-point
    "S" 'daemons-stop-at-point
    "r" 'daemons-reload-at-point
    "R" 'daemons-restart-at-point
    "e" 'daemons-enable-at-point
    "d" 'daemons-disable-at-point
    "t" 'daemons-systemd-toggle-user))

(use-package! vterm
  :custom
  (vterm-always-compile-module t))

(defun my-delete-other-workspace ()
  (interactive)
  (dolist (workspace (+workspace-list-names))
    (unless (eq workspace (+workspace-current-name))
      (+workspace/delete workspace))))

(load! "my-packages/rainbow-indent-and-delimiters.el" doom-user-dir)
(rainbow-indent-and-delimiters-mode 1)

;; TODO Refactor after this
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
  (interactive `(,(read-string "Volume: " nil))))

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
  (man-posframe-show page))

(defun my-open-man-here (page)
  (interactive `(,(read-string "Man: " nil)))
  (man page))

(after! web-mode
  (defun +web/indent-or-yas-or-emmet-expand ()
    "decide if copilot, yas or emmet should expand."
    (interactive)
    (or (emmet-expand-line nil)
        (copilot-accept-completion)
        (indent-relative))))

(defun my-chmod-this-file ( mode )
  (interactive `(,(read-string "File Mode: " nil)))
  (if (and (buffer-file-name) (file-exists-p (buffer-file-name)))
      (shell-command (format "chmod %s %s" mode (buffer-file-name)))
    (message "Buffer has no file.")
    )
  )

(add-hook! 'lsp-mode-hook '(lambda ()
                             (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\tmp\\'")
                             ))

(add-hook! 'projectile-mode-hook '(lambda ()
                                    (add-to-list 'projectile-globally-ignored-directories "tmp")
                                    (add-to-list 'projectile-globally-ignored-directories "vendor")
                                    (add-to-list 'projectile-globally-ignored-directories "CMakeFiles")
                                    (add-to-list 'projectile-globally-ignored-directories "build")
                                    (add-to-list 'projectile-globally-ignored-directories "^.*vendor.*$")
                                    (add-to-list 'projectile-globally-ignored-directories "web-legacy")
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
  (blamer-min-offset 40)
  (blamer-author-formatter "  ✎ %s ")
  (blamer-datetime-formatter "[%s] ")
  :custom-face
  (blamer-face ((t :foreground "#805d96"
                   :background nil
                   :height 80
                   :italic t)))
  :config
  (global-blamer-mode 1))

(defun my-poshandler (info)
  (cons
   (/ (- (plist-get info :parent-frame-width) (plist-get info :posframe-width)) 2) ;x
   (/ (plist-get info :parent-frame-height) 5) ;y
   )
  )

(use-package! vertico-posframe
  :after vertico
  :custom
  (vertico-posframe-parameters
   '((left-fringe . 20)
     (right-fringe . 20)
     ))
  (vertico-posframe-width 100)
  (vertico-posframe-poshandler 'my-poshandler)
  (vertico-posframe-height nil)
  :hook
  (my-new-gui-frame . vertico-posframe-mode)
  )

(use-package! which-key-posframe
  :custom
  (which-key-posframe-poshandler 'my-poshandler)
  (which-key-posframe-parameters
   '((left-fringe . 20)
     (right-fringe . 20)))
  :hook
  (my-new-gui-frame . which-key-posframe-mode))

(load! "my-packages/read-string-posframe.el" doom-user-dir)
(use-package! read-string-posframe
  :hook (my-new-gui-frame . read-string-posframe-mode))

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
            (insert (format "%s %s %s\n" comment-start (file-name-nondirectory file) comment-end)))))))

(after! dired-posframe
  :custom
  (dired-posframe-width 65)
  (dired-posframe-height 25)
  (dired-posframe-min-height nil)
  (dired-posframe-min-width nil)
  (dired-posframe-parameters
   '((left-fringe . 10)
     (right-fringe . 10)))
  :config
  (advice-add 'dired-posframe--show :after 'my-dired-posframe-highlight)
  (advice-add 'keyboard-quit :before 'posframe-delete-all)
  (fset 'dired-posframe--hide 'ignore))

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
      (insert (format "%s\n" line)))))

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
      (insert (format "%s\n" line)))))

(defun my-dired-navigate-into ()
  "Open directory in same dired buffer. Open file in new buffer"
  (interactive)
  (let (
        (file (dired-get-filename nil t)))
    (if (file-directory-p file)
        (dired-find-alternate-file)
      (dired-find-file))))

(evil-define-command my-evil-chmod (mode)
  (interactive "<a>")
  (my-chmod-this-file mode)
  )

(evil-define-command my-evil-man (arg)
  (interactive "<a>")
  (if arg
      (my-open-man arg)
    (call-interactively 'my-open-man)))

(defun my-create-directory (directory)
  "Create a directory recursively using mkdir -p."
  (interactive (list (read-string "Create directory: ")))
  (let ((full-directory (if (file-name-absolute-p directory)
                            directory
                          (expand-file-name directory default-directory))))
    (shell-command (concat "mkdir -p " full-directory))
    (message (concat "Created directory: " full-directory))))

(evil-define-command my-evil-mkdir (arg)
  (interactive "<a>")
  (if arg
      (my-create-directory arg)
    (call-interactively 'my-create-directory)))

(evil-ex-define-cmd "man" 'my-evil-man)
(evil-ex-define-cmd "chmod" 'my-evil-chmod)
(evil-ex-define-cmd "sr" 'projectile-replace-regexp)
(evil-ex-define-cmd "dired" 'dired-jump)
(evil-ex-define-cmd "systemd" 'daemons)
(evil-ex-define-cmd "mkdir" 'my-evil-mkdir)

(use-package! man-posframe
  :ensure t
  :custom
  (man-posframe-width  100)
  (man-posframe-height  50)
  (man-posframe-parameters
   '((left-fringe . 10)
     (right-fringe . 10)
     ))
  )
;; FIX for C and C++ window navigation
(defun ccls-navigate (DIRECTION)
  (cond
   ((string= DIRECTION "D")
    (evil-window-right 1))
   ((string= DIRECTION "L")
    (evil-window-up 1))
   ((string= DIRECTION "R")
    (evil-window-down 1))
   ((string= DIRECTION "U")
    (evil-window-left 1))))

