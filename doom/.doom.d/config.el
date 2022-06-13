;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Ghibran Aljabbar (Ghibranalj)"
      user-mail-address "GhibranResearch@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;kk
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

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

(setq auto-save-default t
      make-backup-files t)


(defun on-new-frame ()
  "This is executed when a new frame is created."
  (+treemacs/toggle)
  ;; (tabify)
  ;;TODO add more
  )

;; packages config
(setq super-save-auto-save-when-idle t)
(global-yascroll-bar-mode 1)

(setq centaur-tabs-set-bar 'under
      centaur-tabs-set-close-button nil
      centaur-tabs-height 42
      )

(custom-set-faces!
        '(treemacs-root-face :foreground "#F78C6C" )
        '(doom-themes-treemacs-root-face :foreground "#F78C6C" )
  )

; (custom-set-faces!
  ;'(centaur-tabs-active-bar-face ((t ( :background "#F78C6C" )))
   ;                              ))

(setq highlight-indent-guides-method 'character)

(defun copilot-tab ()
  "Tab complete autopilot."
  (interactive)
  (or (copilot-accept-completion)
      (company-indent-or-complete-common nil)))

(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind (("C-TAB" . 'copilot-accept-completion-by-word)
         ("C-<tab>" . 'copilot-accept-completion-by-word)
         :map company-active-map
         ("<tab>" . 'copilot-tab)
         ("TAB" . 'copilot-tab)
         :map company-mode-map
         ("<tab>" . 'copilot-tab)
         ("TAB" . 'copilot-tab)))

(after! company
  (setq +lsp-company-backends '(company-tabnine :separate company-capf company-yasnippet))
  (setq company-show-quick-access t)
  (setq company-idle-delay 0)
  )

(after! magit
  (setq magit-diff-refine-hunk 'all)
  )

(map!
 :leader
 (:prefix ("t" . "toggle")
  :desc "Toggle minimap" "m" #'minimap-mode))

(setq
 ;; Configure minimap position
 minimap-window-location 'right ; Minimap on the right side
 minimap-width-fraction 0.0 ; slightly smaller minimap
 minimap-minimum-width 10 ; also slightly smaller minimap
                                        ; seems to work better
 minimap-enlarge-certain-faces nil ; enlarge breaks BlockFont
 )
(custom-set-faces!
  '(minimap-font-face :height 12 :group 'minimap))

(add-hook! window-selection-change-functions
  'minimap-mode)
(map!
 :leader
 (:prefix ("b" . "buffer")
  :desc "Format buffer" "f" #'format-all-buffer))

(use-package! company-box
  :hook (company-mode . company-box-mode))

;; Running on daemon startup

(if (daemonp)
    (add-hook 'after-make-frame-functions (lambda (frame)
                                            (with-selected-frame frame
                                              (on-new-frame))))
  (on-new-frame))

(add-hook! 'prog-mode-hook #'format-all-mode)
(add-hook! 'prog-mode-hook #'highlight-indent-guides-mode)
(super-save-mode +1)
(minimap-mode 1)
(beacon-mode 1)
;; (awesome-tab-mode 1)

                                        ;end of file
