;;; ../.dotfiles/home/.doom.d/read-string.el -*- lexical-binding: t; -*-

;; (read-from-minibuffer PROMPT &optional INITIAL-CONTENTS KEYMAP READ HIST
;; DEFAULT-VALUE INHERIT-INPUT-METHOD)

(defun my-revert-vertico-posframe()
  (setq vertico-posframe-width 100)
  (setq vertico-posframe-height nil)
  (setq vertico-count-format (cons "%-6s " "%s/%s"))
  (setq vertico-posframe-poshandler 'my-poshandler)
  (advice-remove 'vertico-insert 'my-vertico-insert-tab)
  )

(defun my-vertico-insert-tab ()
  (if vertico-posframe-height
      (progn
        (setq vertico-posframe-height 10)
        )
    )
  )

(add-hook! 'minibuffer-exit-hook  'my-revert-vertico-posframe)

(defun read-string ( PROMPT &optional INITIAL-INPUT HISTORY DEFAULT-VALUE INHERIT-INPUT-METHOD KEYMAP)
  (setq vertico-posframe-height 1)
  (setq vertico-posframe-width 40)
  (setq vertico-count-format  (cons "%-0s" ""))
  (advice-add 'vertico-insert :before 'my-vertico-insert-tab)
  (completing-read PROMPT (symbol-value HISTORY) nil nil INITIAL-INPUT HISTORY DEFAULT-VALUE INHERIT-INPUT-METHOD)
  )

(evil-define-command evil-ex (&optional initial-input)
  :keep-visual t
  :repeat abort
  (interactive
   (let ((s (concat
             (cond
              ((and (evil-visual-state-p)
                    evil-ex-visual-char-range
                    (memq (evil-visual-type) '(inclusive exclusive)))
               "`<,`>")
              ((evil-visual-state-p) "'<,'>")
              (current-prefix-arg
               (let ((arg (prefix-numeric-value current-prefix-arg)))
                 (cond ((< arg 0) (setq arg (1+ arg)))
                       ((> arg 0) (setq arg (1- arg))))
                 (if (= arg 0) "."
                   (format ".,.%+d" arg)))))
             evil-ex-initial-input)))
     (list (when (> (length s) 0) s))))

  (let ((evil-ex-current-buffer (current-buffer))
        (evil-ex-previous-command (unless initial-input
                                    (car evil-ex-history)))
        evil-ex-argument-handler result)
    (minibuffer-with-setup-hook
        (lambda ()
          (evil-ex-setup)
          (when initial-input (evil-ex-update)))
      (setq result
            (read-string
             ":" ;; prompt
             (or initial-input
                 (and evil-ex-previous-command
                      evil-want-empty-ex-last-command
                      (propertize evil-ex-previous-command 'face 'shadow))) ;; initial-input
             'evil-ex-history ;; hist
             (when evil-want-empty-ex-last-command evil-ex-previous-command) ;; def
             t
             evil-ex-completion-map
             ))) ;; inherit-input-method
    (evil-ex-execute result)))

(defun magit-read-string (prompt &optional initial-input history default-value
                                 inherit-input-method no-whitespace)
  (when default-value
    (when (consp default-value)
      (setq default-value (car default-value)))
    (unless (stringp default-value)
      (setq default-value nil)))
  (let* ((minibuffer-completion-table nil)
         (val (read-string
               (magit-prompt-with-default (concat prompt ": ") default-value) ;; prompt
               initial-input ;; initial
               history ;; history
               default-value ;; default
               inherit-input-method ;; inherit-input-method
               ))
         (trim (lambda (regexp string)
                 (save-match-data
                   (if (string-match regexp string)
                       (replace-match "" t t string)
                     string)))))
    (when (and (string= val "") default-value)
      (setq val default-value))
    (when no-whitespace
      (setq val (funcall trim "\\`\\(?:[ \t\n\r]+\\)"
                         (funcall trim "\\(?:[ \t\n\r]+\\)\\'" val))))
    (cond ((string= val "")
           (user-error "Need non-empty input"))
          ((and no-whitespace (string-match-p "[\s\t\n]" val))
           (user-error "Input contains whitespace"))
          (t val))))
