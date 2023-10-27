;;; ../.dotfiles/home/.doom.d/my-packages/my-crazy-spotify-emacs.el -*- lexical-binding: t; -*-
(require 'smudge)
(require 'simple)
;; TODO make a package out of this

(defun my-is-service-active-p (service &optional root)
  "Return t if SERVICE is active. use --user if ROOT is nil"
  (let ((active (s-trim
                 (shell-command-to-string
                  (format "systemctl is-active %s %s" (if root "" "--user") service)))))
    (string= active "active")))

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
  (interactive `(,(read-string "Volume: " nil)))
  (if (stringp volume)
      (setq volume (string-to-number volume)))
  (when (and  smudge-selected-device-id (<= volume 100) (>= volume 0) )
    (smudge-api-set-volume smudge-selected-device-id volume))
  )

