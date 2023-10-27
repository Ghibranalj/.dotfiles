;:; -*- lexical-binding: t; -*-

(require 'vertico-posframe)
(require 'evil)
(require 'magit)

(defgroup read-string-posframe nil
  "show readstring, evil ex and magit read string in posframe."
  :group 'convenience)

(defcustom read-string-posframe-width 40
  "How wide read-string-posframe is"
  :group 'read-string-posframe
  :type 'integer)

(defcustom read-string-posframe-height 1
  "How tall read-string-posgrame is"
  :group 'read-string-posframe
  :type 'integer)

(defun read-string-posframe--read-string ( PROMPT &optional INITIAL-INPUT HISTORY DEFAULT-VALUE INHERIT-INPUT-METHOD KEYMAP)
  (advice-remove 'read-from-minibuffer #'read-string-posframe--read-from-minibuffer)
  (let ((height-before vertico-posframe-height)
        (width-before vertico-posframe-width)
        (count-format-before vertico-count-format))
    (unwind-protect
        (progn
          (setq vertico-posframe-height read-string-posframe-height
                vertico-posframe-width read-string-posframe-width
                vertico-count-format  (cons "%-0s" ""))
          (completing-read PROMPT (symbol-value HISTORY) nil nil INITIAL-INPUT HISTORY DEFAULT-VALUE INHERIT-INPUT-METHOD))
      (setq vertico-posframe-height height-before
            vertico-posframe-width width-before
            vertico-count-format count-format-before))))

(defun read-string-posframe--read-from-minibuffer (prompt &optional initial keymap read history default inherit-input-method)
  (read-string-posframe--read-string prompt initial history default inherit-input-method keymap))

(defun read-string-posframe--replace-read-from-minibuffer (orig-fn &rest args)
  (advice-add 'read-from-minibuffer :override #'read-string-posframe--read-from-minibuffer)
  (unwind-protect
      (apply orig-fn args)
    (advice-remove 'read-from-minibuffer #'read-string-posframe--read-from-minibuffer)))

(define-minor-mode read-string-posframe-mode
  "Readstirng from a popup"
  :lighter " ReadStringPosframe"
  :global t
  (if read-string-posframe-mode
      (progn
        (advice-add 'evil-ex :around #'read-string-posframe--replace-read-from-minibuffer)
        (advice-add 'magit-read-string :around #'read-string-posframe--replace-read-from-minibuffer)
        (advice-add 'read-string :override #'read-string-posframe--read-string))
    (progn
        (advice-remove 'evil-ex #'read-string-posframe--replace-read-from-minibuffer)
        (advice-remove 'magit-read-string #'read-string-posframe--replace-read-from-minibuffer)
        (advice-remove 'read-string #'read-string-posframe--read-string))))

(provide 'read-string-posframe)
;;; read-string-posframe-mode.el ends here
