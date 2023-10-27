;;; ../.dotfiles/home/.doom.d/my-packages/ssh-remote.el -*- lexical-binding: t; -*-
(require 'persist)

(persist-defvar ssh-remote--ssh-user-history nil
                "History for ssh user."
                (concat doom-cache-dir "/ssh-remote"))

(persist-defvar ssh-remote--ssh-host-history nil
                "History for ssh host."
                (concat doom-cache-dir "/ssh-remote"))

(defun ssh-remote--read-string-hist (prompt &optional hist)
  "Read a string from the minibuffer with PROMPT. History is stored in HIST."
  (let* ((height (+ (length hist) 1))
         (prev-height vertico-posframe-height)
         (result (unwind-protect
                     (progn
                       (setq vertico-posframe-height height)
                       (completing-read prompt hist nil nil))
                   (setq vertico-posframe-height prev-height))))
    result))

;;;###autoload
(defun ssh-remote-connect()
  "Connect to remote ssh in a new workspace."
  (interactive)
  (let* (
         (user (ssh-remote--read-string-hist "User (ssh): " ssh-remote--ssh-user-history))
         (host (ssh-remote--read-string-hist "Host (ssh): " ssh-remote--ssh-host-history))
         (conn-str (format "%s@%s" user host)))
    (+workspace-switch conn-str t)
    (if (dired (format "/scp:%s:" conn-str))
        (progn
          (add-to-list 'ssh-remote--ssh-user-history user)
          (add-to-list 'ssh-remote--ssh-host-history host)
          (persist-save ssh-remote--ssh-user-history)
          (persist-save ssh-remote--ssh-host-history))
      (progn
        (message "Failed to connect to %s" conn-str)
        (+workspace/delete conn-str)))))
