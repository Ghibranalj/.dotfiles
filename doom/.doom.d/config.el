
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

(defun on-new-frame ()
  "This is executed when a new frame is created."
  ;; (+treemacs/toggle)
  ;; (+neotree/open)
  (if window-system
      (+my/setup-browser))
  ;; (copilot-diagnose)
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

(map!
 :leader
 :desc "Switch buffer in workspace" "," #'+my/consult-workspace
 (:prefix ("b" . "buffer")
  :desc "Format buffer" "f" #'+format/buffer
  :desc "Switch to browser" "w" #'+my/consult-browser
  :desc "Switch to terminal in workspace" "t" #'+my/consult-terminal
  )

 (:prefix ("c" . "code")
  :desc "Comment line" "c" #'+my/comment-or-uncomment
  :desc "Compile" "C" #'compile
  :desc "Format buffer" "f" #'+format/buffer
  :desc "List all occurance" "l" #'helm-swoop
  )

 (:prefix ("o" . "open")
  :desc "Open manpage" "m" #'man
  :desc "Open browser" "w" #'eaf-open-browser-with-history
  :desc "Open browser bookmark" "W" #'eaf-open-bookmark
  :desc "Open google" "g" #'+my/google-search
  :desc "Open github" "G" #'+my/open-github
  )

 (:prefix ("t" . "toggle")
  :desc "Toggle minimap" "m" #'minimap-mode
  :desc "Toggle zen-mode" "z" #'+zen/toggle)

 (:prefix ("e" . "eval")
  :desc "Evaluate buffer" "b" #'eval-buffer
  :desc "Evaluate region" "r" #'eval-region
  :desc "Evaluate line" "l" #'+my/eval-line)
 )

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
(add-hook! 'prog-mode-hook #'format-all-mode)

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


;; eaf and browser
(defun +my/setup-browser ()
  "Setup eaf and browser."
  (message "Browser is being setup")
  (add-to-list 'load-path "~/.emacs.d/site-lisp/emacs-application-framework/")
  (require 'eaf)
  (require 'eaf-browser)
  (eaf-setq eaf-browser-enable-bookmark "true")
  (eaf-setq eaf-browser-enable-adblocker "true")
  (defvar eaf-browser-default-search-engine "google")
  )

(defun +my/open-browser(url &optional args)
  "Open URL with ARGS on eaf-browser when not terminal, chrome when terminal."
  (if window-system
      (eaf-open-browser url args)
    (browse-url-chrome url args)
    ))

(setq browse-url-browser-function '+my/open-browser)

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
                     (eq (buffer-local-value 'major-mode x) 'eaf-mode))
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
  (list :name    "Current workspace"
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
                      )
                     )
                   (buffer-list))))))

(after! consult
  (add-to-list 'consult-buffer-sources '+my/consult--eaf-source 'append)
  (add-to-list 'consult-buffer-sources '+my/consult--terminal-source 'append)
  (add-to-list 'consult-buffer-sources '+my/consult--workspace-source 'append)
  )

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
   '(+my/consult--workspace-source +my/consult--terminal-source +my/consult--eaf-source))
  :requre-match
  (confirm-nonexistent-file-or-buffer)
  :prompt
  (format "Switch to buffer (%s)" (+workspace-current-name))
  :sort nil
  :history 'consult--buffer-history
  )

;; EOF
