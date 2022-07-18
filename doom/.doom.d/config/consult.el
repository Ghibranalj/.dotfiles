;;; ../.dotfiles/doom/.doom.d/config/consult.el -*- lexical-binding: t; -*-

(require 'consult)

(defvar +my/consult--eaf-source
  (list :name     "Browser"
        :category 'buffer
        :narrow   ?o
        :face     'consult-buffer
        :history  'buffer-name-history
        :state    #'consult--buffer-state
        :require-match t
        :items
        (lambda ()
          (mapcar #'buffer-name
                  (seq-filter
                   (lambda (x)
                     (and (eq (buffer-local-value 'major-mode x) 'eaf-mode)
                          (+workspace-contains-buffer-p x)
                          ))
                   (buffer-list))))))


(defvar +my/consult--terminal-source
  (list :name     "Terminal"
        :category 'buffer
        :narrow   ?o
        :face     'consult-buffer
        :history  'buffer-name-history
        :state    #'consult--buffer-state
        :require-match t
        :items
        (lambda ()
          (mapcar #'buffer-name
                  (seq-filter
                   (lambda (x)
                     (and
                      (eq (buffer-local-value 'major-mode x) 'vterm-mode)
                      (+workspace-contains-buffer-p x)
                      )
                     )
                   (buffer-list))))))

(defvar +my/consult--workspace-source
  (list :name    "Buffers"
        :category 'buffer
        :narrow   ?o
        :face     'consult-buffer
        :history  'buffer-name-history
        :state    #'consult--buffer-state
        :require-match t
        :items
        (lambda ()
          (mapcar #'buffer-name
                  (seq-filter
                   (lambda (x)
                     (and
                      (+workspace-contains-buffer-p x)
                      (not (eq (buffer-local-value 'major-mode x) 'vterm-mode))
                      (not (eq (buffer-local-value 'major-mode x) 'eaf-mode))
                      )
                     )
                   (buffer-list))))))

(after! consult
  (add-to-list 'consult-buffer-sources '+my/consult--eaf-source 'append)
  (add-to-list 'consult-buffer-sources '+my/consult--terminal-source 'append)
  (add-to-list 'consult-buffer-sources '+my/consult--workspace-source 'append)
  )

(defun +my/consult-browser ()
  "Open eaf-browser."
  (interactive)
  (consult-buffer '(+my/consult--eaf-source)))

(defun +my/consult-terminal ()
  "Open terminal."
  (interactive)
  (consult-buffer '(+my/consult--terminal-source)))

(defun +my/consult-workspace ()
  "Switch to buffer in workspace.
Shows terminal in seperate section. Also shows browsers."
  (interactive)
  (consult--multi
   '(+my/consult--workspace-source +my/consult--terminal-source +my/consult--eaf-source)
   :require-match
   (confirm-nonexistent-file-or-buffer)
   :prompt (format "Switch to buffer (%s): "
                   (+workspace-current-name))
   :history 'consult--buffer-history
   :sort nil)
  )
