;;; ../.dotfiles/doom/.doom.d/keymap.el -*- lexical-binding: t; -*-

(map!
 :nv  "M-k" #'drag-stuff-up
 :nv "M-j" #'drag-stuff-down
 :nv  "M-<up>" #'drag-stuff-up
 :nv "M-<down>" #'drag-stuff-down

 :n "<f11>" #'evil-window-left
 :n "<f12>" #'evil-window-right
 "C-/" #'my-comment-or-uncomment

 :leader
 :desc "Switch buffer in workspace" "," #'my-consult-workspace
 :desc "Yank from kill ring" "y" #'yank-from-kill-ring
 (:prefix ("b" . "buffer")
  :desc "Format buffer" "f" #'+format/buffer
  :desc "Switch to browser" "w" #'my-consult-browser
  :desc "Switch to terminal in workspace" "t" #'my-consult-terminal
  ;; :desc "Search current buffer" "s" #'+default/search-buffer
  )

  (:prefix ("c" . "code")
   :desc "Comment line" "c" #'my-comment-or-uncomment
   :desc "Compile" "C" #'compile
   :desc "Format buffer" "f" #'+format/buffer
   :desc "List all occurance" "l" #'helm-swoop
   :desc "list all matches in project" "m" #'sidekick-at-point
   :desc "Emmet expand" "TAB" #'emmet-expand-line
   )

  (:prefix ("o" . "open")
   :desc "Open manpage" "M" #'man
   :desc "Open manpage" "m" #'my-open-man
   :desc "Open browser" "w" #'eaf-open-browser-with-history
   :desc "Open browser bookmark" "W" #'eaf-open-bookmark
   :desc "Open google" "g" #'my-google-search
   :desc "Open github" "G" #'my-open-github
   :desc "Open ssh connection" "s" #'my-connect-remote-ssh
   :desc "open dired" "/" #'dired-jump
   :desc "Open init daemon" "i" #'daemons
   :desc "Open selected link in browser" "l" #'my-open-selected-link
   )

  (:prefix ("t" . "toggle")
   :desc "Toggle minimap" "m" #'minimap-mode
   :desc "Toggle zen-mode" "z" #'+zen/toggle)

  (:prefix ("e" . "eval")
   :desc "Evaluate buffer" "b" #'eval-buffer
   :desc "Evaluate region" "r" #'eval-region
   :desc "Evaluate line" "l" #'my-eval-line
   :desc "Evaluate last sexpr" "e" #'eval-last-sexp
   )

  (:prefix ("TAB" . "workspace")
   :desc "Save current worksppace to file" "S" #'my-save-current-workspace
   :desc "Delete other workspace" "D" #'my-delete-other-workspace
   :desc "Add buffer to workspace" "a" #'persp-add-buffer
   )

  (:prefix-map ("s" . "search")
   :desc "find file in current directory" "f" #'my-find-file-in-directory)

  (:prefix-map ("g" . "git")
   :desc "Forge pull" "p" #'forge-pull)

  (:prefix-map ("v" . "Verb")
   :desc "Send request at point" "v" #'verb-send-request-on-point
   :desc "Send request on new window" "r" #'verb-send-request-on-point-other-window
   :desc "Kill reponse window and buffer" "k" #'verb-kill-all-response-buffers
   :desc "Send request ignore reponse" "R" #'verb-send-request-on-point-no-window
   :desc "Show headers" "h" #'verb-toggle-show-headers)

  (:prefix-map ("m" . "Music")
   :desc "Start music" "m" #'my-start-smudge
   :desc "Search music" "s" #'smudge-track-search
   :desc "Toggle music" "T" #'my-pause-music-start-again
   :desc "Toggle music" "t" #'smudge-controller-toggle-play
   :desc "Select device" "d" #'smudge-select-device
   :desc "Volume up" "=" #'smudge-controller-volume-up
   :desc "Volume down" "-" #'smudge-controller-volume-down
   :desc "Volume mute" "0" #'smudge-controller-volume-mute-unmute
   :desc "My playlist" "p" #'smudge-my-playlists
   :desc "Reload daemon" "r" #'my-reload-spotifyd
   :desc "Next track"  "]" #'smudge-controller-next-track
   :desc "Previous track" "[" #'smudge-controller-previous-track
   :desc "Set volume" "v" #'my-smudge-set-volume
   )
  )
