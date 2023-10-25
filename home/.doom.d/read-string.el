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

(defun my-read-from-minibuffer (prompt &optional initial keymap read history default inherit-input-method)
       (read-string prompt initial history default inherit-input-method keymap))

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
                 (if (= arg 0) "." (format ".,.%+d" arg)))))
             evil-ex-initial-input)))
     (list (unless (string= s "") s))))
  (let ((buffer (current-buffer))
        (previous-command (when evil-want-empty-ex-last-command
                            (car evil-ex-history)))
        s evil--ex-expression evil--ex-cmd evil--ex-argument-handler)
    (minibuffer-with-setup-hook
        (lambda ()
          (setq-local evil-ex-original-buffer buffer)
          (evil-ex-setup)
          (if initial-input (evil--ex-update)
            (when previous-command
              (add-hook 'pre-command-hook #'evil-ex-remove-default nil t))))
      (setq s (my-read-from-minibuffer
               ":"
               (or initial-input
                   (and previous-command (propertize previous-command 'face 'shadow)))
               evil-ex-completion-map nil 'evil-ex-history nil t)))
    (if evil--ex-expression
        (eval evil--ex-expression t)
      (when (string= s "") (setq s previous-command))
      (unless (= (length s) 0) (evil-ex-execute s)))))

(defun magit-read-string ( prompt &optional initial-input history default-value
                           inherit-input-method no-whitespace)
  (when default-value
    (when (consp default-value)
      (setq default-value (car default-value)))
    (unless (stringp default-value)
      (setq default-value nil)))
  (let* ((minibuffer-completion-table nil)
         (val (my-read-from-minibuffer
               (magit-prompt-with-default (concat prompt ": ") default-value)
               initial-input (and no-whitespace magit-minibuffer-local-ns-map)
               nil history default-value inherit-input-method))
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
