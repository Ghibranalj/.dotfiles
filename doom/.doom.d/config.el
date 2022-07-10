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
  :bind (("C-TAB" . 'copilot-accept-completion-by-word)
         ("C-<tab>" . 'copilot-accept-completion-by-word)
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
 (:prefix ("b" . "buffer")
  :desc "Format buffer" "f" #'+format/buffer
  :desc "Switch buffer" "b" #'switch-to-buffer
  :desc "Switch workspace buffer" "B" #'+vertico/switch-workspace-buffer
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
  :desc "Evaluate line" "l" #'eval-line-by-line)

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
(lsp-headerline-breadcrumb-mode 1)

(setq scroll-margin 10)

;; misc hook
(use-package! company-box
  :hook (company-mode . company-box-mode))
(add-hook! 'prog-mode-hook #'format-all-mode)

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
  (setq browse-url-browser-function 'eaf-open-browser)
  )

(defun +my/google-search ()
  "Search Google inside eaf-browser."
  (interactive)
  (eaf-open-browser
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
    (eaf-open-browser "github.com")
    ))

;;EOF
