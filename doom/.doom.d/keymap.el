;;; ../.dotfiles/doom/.doom.d/keymap.el -*- lexical-binding: t; -*-

(map!

 :nv  "M-k" #'drag-stuff-up
 :nv "M-j" #'drag-stuff-down
 :nv  "M-<up>" #'drag-stuff-up
 :nv "M-<down>" #'drag-stuff-down

 :ni "M-<mouse-1>"  #'mc/add-cursor-on-click

 :n "<f11>" #'evil-window-left
 :n "<f12>" #'evil-window-right
 "C-/" #'+my/comment-or-uncomment

 :leader
 :desc "Switch buffer in workspace" "," #'+my/consult-workspace
 (:prefix ("b" . "buffer")
  :desc "Format buffer" "f" #'+format/buffer
  :desc "Switch to browser" "w" #'+my/consult-browser
  :desc "Switch to terminal in workspace" "t" #'+my/consult-terminal
  :desc "Search current buffer" "s" #'+default/search-buffer)

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
  :desc "open dired" "/" #'dired-jump
  :desc "Open init daemon" "i" #'daemons
  )

 (:prefix ("t" . "toggle")
  :desc "Toggle minimap" "m" #'minimap-mode
  :desc "Toggle zen-mode" "z" #'+zen/toggle)


 (:prefix ("e" . "eval")
  :desc "Evaluate buffer" "b" #'eval-buffer
  :desc "Evaluate region" "r" #'eval-region
  :desc "Evaluate line" "l" #'+my/eval-line
  :desc "Evaluate last sexpr" "e" #'eval-last-sexp)

 (:prefix ("TAB" . "workspace")
  :desc "Save current worksppace to file" "S" #'+my/save-current-workspace
  :desc "Delete other workspace" "D" #'+my/delete-other-workspace
  )
      ;;; <leader> m --- multiple cursors
 (:when (featurep! :editor multiple-cursors)
  (:prefix-map ("m" . "multiple-cursors")
   :desc "Edit lines"         "l"         #'mc/edit-lines
   :desc "Mark next"          "n"         #'mc/mark-next-like-this
   :desc "Unmark next"        "N"         #'mc/unmark-next-like-this
   :desc "Mark previous"      "p"         #'mc/mark-previous-like-this
   :desc "Unmark previous"    "P"         #'mc/unmark-previous-like-this
   :desc "Mark all"           "t"         #'mc/mark-all-like-this
   :desc "Mark all DWIM"      "m"         #'mc/mark-all-like-this-dwim
   :desc "Edit line endings"  "e"         #'mc/edit-ends-of-lines
   :desc "Edit line starts"   "a"         #'mc/edit-beginnings-of-lines
   :desc "Mark tag"           "s"         #'mc/mark-sgml-tag-pair
   :desc "Mark in defun"      "d"         #'mc/mark-all-like-this-in-defun
   :desc "Add cursor w/mouse" "<mouse-1>" #'mc/add-cursor-on-click))


(:prefix-map ("s" . "search")
 :desc "find file in current directory" "f" #'+my/find-file-in-directory)
 )
