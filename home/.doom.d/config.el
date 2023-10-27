;;; $DOOM_DIR/config.el -*- lexical-binding: t; -*-
(load! "keymap.el" doom-user-dir)
;; find file subdirectory my-packages
;; and eval every single one
(let ((d (expand-file-name "my-packages" doom-user-dir)))
  (dolist (file (directory-files d t "\\.el$"))
    (load! file)))

(defvar my-new-frame-hook nil
  "Hook run after a any new frame is created.")
(defvar my-new-gui-frame-hook nil
  "Hook run after a any new gui frame is created.")
(defvar my-gui-already-started nil
  "Flag to check if gui has already been started.")
(defun on-new-frame ()
  "This is executed when a new frame is created."
  (if window-system
      (progn
        (unless my-gui-already-started
          (setq my-gui-already-started t)
          (run-hooks 'my-new-gui-frame-hook)))))
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
 ;; Doom Variables
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
(use-package! man
  :custom
  (Man-notify-method 'pushy)
  :hook
  (Man-mode 'my-add-buffer-to-project))

(use-package! evil-megasave
  :hook
  (prog-mode . evil-megasave-mode)
  (git-commit-mode . evil-megasave-mode)
  (conf-mode . evil-megasave-mode)
  (yaml-mode . evil-megasave-mode))

;; tremacs colors
(use-package treemacs
  :config
  (custom-set-faces!
    '(treemacs-root-face :foreground "#F78C6C")
    '(doom-themes-treemacs-root-face :foreground "#F78C6C")))

;; Coplilot
(use-package! copilot
  :config
  (defun +copilot/tab ()
    "Copilot completion."
    (interactive)
    (or
     (if (bound-and-true-p emmet-mode)
         (emmet-expand-line nil))
     (copilot-accept-completion)
     (indent-relative)))
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
  (prog-mode . lsp-headerline-breadcrumb-mode)
  (lsp-mode . (lambda ()
                (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\tmp\\'")))

  :config
  ;; GLSL language support
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '("glslls" "--stdin"))
                    :activation-fn (lsp-activate-on "glsl")
                    :server-id 'glslls))
  (add-to-list 'lsp-language-id-configuration
               '(glsl-mode . "glsl")))

(use-package! magit
  :custom
  (magit-clone-default-directory "~/Workspace/")
  ;; :hook
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
  (add-to-list 'consult-buffer-sources 'my-consult--dired-source 'append))

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

(add-hook! 'my-new-frame-hook
           '(lambda ()
              (dolist (x persist--symbols)
                (persist-load x))))

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
  (defun my-dired-navigate-into ()
    "Open directory in same dired buffer. Open file in new buffer"
    (interactive)
    (let (
          (file (dired-get-filename nil t)))
      (if (file-directory-p file)
          (dired-find-alternate-file)
        (dired-find-file))))

  (evil-collection-define-key 'normal 'dired-mode-map
    "h" '(lambda () (interactive) (find-alternate-file ".."))
    ;; "l" 'dired-find-alternate-file
    "l" 'my-dired-navigate-into
    "." 'dired-hide-dotfiles-mode
    "," 'dired-posframe-show
    "s" 'my-dired-posframe-scroll-down
    "w" 'my-dired-posframe-scroll-up
    "e" 'lsp-dired-mode)
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


(define-generic-mode 'xmodmap-mode
  '(?!)
  '("add" "clear" "keycode" "keysym" "pointer" "remove")
  nil
  '("[xX]modmap\\(rc\\)?\\'")
  nil
  "Simple mode for xmodmap files.")
(add-hook! 'xmodmap-mode-hook 'display-line-numbers-mode)

(use-package! cc-mode
  :hook
  (c-mode . (lambda ()(c-toggle-comment-style -1))))

(use-package! ccls
  :config
  (defun ccls-navigate (DIRECTION)
    (cond
     ((string= DIRECTION "D")
      (evil-window-right 1))
     ((string= DIRECTION "L")
      (evil-window-up 1))
     ((string= DIRECTION "R")
      (evil-window-down 1))
     ((string= DIRECTION "U")
      (evil-window-left 1)))))

(defun my-open-man (page)
  (interactive `(,(read-string "Man: " nil)))
  (man-posframe-show page ))

(defun my-open-man-here (page)
  (interactive `(,(read-string "Man: " nil)))
  (man page))

(use-package! web-mode
  :config
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
    (message "Buffer has no file.")))

(use-package! projectile
  :config
  (add-to-list 'projectile-globally-ignored-directories "tmp")
  (add-to-list 'projectile-globally-ignored-directories "vendor")
  (add-to-list 'projectile-globally-ignored-directories "CMakeFiles")
  (add-to-list 'projectile-globally-ignored-directories "build")
  (add-to-list 'projectile-globally-ignored-directories "^.*vendor.*$")
  (add-to-list 'projectile-globally-ignored-directories "web-legacy"))

(defun my-dap-debug-last()
  (interactive)
  (call-interactively '+make/run-last)
  (call-interactively 'dap-debug-last))

(defun my-dap-debug ()
  (interactive)
  (call-interactively '+make/run)
  (call-interactively 'dap-debug))

(use-package! dap-mode
  :custom
  ;; (sessions locals breakpoints expressions controls tooltip)
  (dap-auto-configure-features '(locals controls tooltip)))

;; Git Blame
(use-package blamer
  :bind (("s-i" . blamer-show-commit-info))
  :defer 20
  :custom
  (blamer-idle-time 0.2)
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
   ;; X
   (/ (- (plist-get info :parent-frame-width) (plist-get info :posframe-width)) 2)
   ;; Y
   (/ (plist-get info :parent-frame-height) 5)))

(use-package! vertico-posframe
  :after vertico
  :custom
  (vertico-posframe-parameters
   '((left-fringe . 20)
     (right-fringe . 20)
     ))
  (vertico-posframe-width 100)
  (vertico-posframe-height nil)
  (vertico-posframe-poshandler 'my-poshandler)
  :hook
  (my-new-gui-frame . vertico-posframe-mode))

(defun my-create-directory (directory)
  "Create a directory recursively using mkdir -p."
  (interactive (list (read-string "Create directory: ")))
  (let ((full-directory (if (file-name-absolute-p directory)
                            directory
                          (expand-file-name directory default-directory))))
    (shell-command (concat "mkdir -p " full-directory))
    (message (concat "Created directory: " full-directory))))

(use-package! evil
  :config
  (evil-define-command my-evil-mkdir (arg)
    (interactive "<a>")
    (if arg
        (my-create-directory arg)
      (call-interactively 'my-create-directory)))
  (evil-define-command my-evil-man (arg)
    (interactive "<a>")
    (if arg
        (my-open-man arg)
      (call-interactively 'my-open-man)))
  (evil-define-command my-evil-chmod (mode)
    (interactive "<a>")
    (my-chmod-this-file mode))
  (evil-ex-define-cmd "man" 'my-evil-man)
  (evil-ex-define-cmd "chmod" 'my-evil-chmod)
  (evil-ex-define-cmd "sr" 'projectile-replace-regexp)
  (evil-ex-define-cmd "dired" 'dired-jump)
  (evil-ex-define-cmd "systemd" 'daemons)
  (evil-ex-define-cmd "mkdir" 'my-evil-mkdir))

(use-package! read-string-posframe
  :hook
  (my-new-gui-frame . read-string-posframe-mode))

(use-package! which-key-posframe
  :custom
  (which-key-posframe-poshandler 'my-poshandler)
  (which-key-posframe-parameters
   '((left-fringe . 10)
     (right-fringe . 20)
     ))
  :hook
  (my-new-gui-frame . which-key-posframe-mode))

(use-package! man-posframe
  :custom
  (man-posframe-width  100)
  (man-posframe-height  30))

(rainbow-indent-and-delimiters-mode 1)

(use-package! smudge
  :init
  (if (file-exists-p "~/.spotify-secrets.el")
      (load! "~/.spotify-secrets.el")
    (progn
      (message "No spotify secrets found.")
      (setq spotify-client-id nil)
      (setq spotify-client-secret nil)))
  :custom
  (smudge-oauth2-callback-port "3725")
  (smudge-oauth2-client-id  spotify-client-id)
  (smudge-oauth2-client-secret spotify-client-secret)
  :config
  (global-smudge-remote-mode 1)
  ;;
  (defun my-pause-music-start-again (time)
    (interactive '("2 min"))
    (unless (featurep 'smudge)
      (my-start-smudge))
    (smudge-api-pause)
    (run-at-time time nil #'smudge-api-play))
  ;;
  (defun my-smudge-set-volume (volume)
    (interactive `(,(read-string "Volume: " nil)))
    (if (stringp volume)
        (setq volume (string-to-number volume)))
    (when (and  smudge-selected-device-id (<= volume 100) (>= volume 0) )
      (smudge-api-set-volume smudge-selected-device-id volume)))
  :bind
  (:map smudge-track-search-mode-map
        ("RET" . smudge-track-select)))
