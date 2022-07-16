;;; ../.dotfiles/doom/.doom.d/keymap.el -*- lexical-binding: t; -*-

(map!
 :nv  "M-k" #'drag-stuff-up
 :nv "M-j" #'drag-stuff-down
 :nv  "M-<up>" #'drag-stuff-up
 :nv "M-<down>" #'drag-stuff-down

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
  :desc "list all matches in project" "m" #'sidekick-at-point
  )

 (:prefix ("o" . "open")
  :desc "Open manpage" "m" #'man
  :desc "Open browser" "w" #'eaf-open-browser-with-history
  :desc "Open browser bookmark" "W" #'eaf-open-bookmark
  :desc "Open google" "g" #'+my/google-search
  :desc "Open github" "G" #'+my/open-github
  :desc "Open ssh connection" "s" #'+my/connect-remote-ssh
  )

 (:prefix ("t" . "toggle")
  :desc "Toggle minimap" "m" #'minimap-mode
  :desc "Toggle zen-mode" "z" #'+zen/toggle)


 ;; (:prefix ("e" . "eval")
 ;;  :desc "Evaluate buffer" "b" #'eval-buffer
 ;;  :desc "Evaluate region" "r" #'eval-region
 ;;  :desc "Evaluate line" "l" #'+my/eval-line)

 (:prefix ("TAB" . "workspace")
  :desc "Save current worksppace to file" "S" #'+my/save-current-workspace
  )
 )
