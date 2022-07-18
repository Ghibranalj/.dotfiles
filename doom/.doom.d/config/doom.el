;;; ../.dotfiles/doom/.doom.d/config/doom.el -*- lexical-binding: t; -*-


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
