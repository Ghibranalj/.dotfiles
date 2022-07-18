;;; ../.dotfiles/doom/.doom.d/config/minimap.el -*- lexical-binding: t; -*-

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
