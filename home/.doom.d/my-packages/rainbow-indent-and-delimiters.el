(require 'rainbow-delimiters)
(require 'highlight-indent-guides)

(defgroup rainbow-indent-and-delimeters nil
  "Rainbow delimeters and indentation."
  :group 'visual)

(defface rainbow-indent-and-delimeters-face1
  '((t (:foreground "#B23F77")))
  "Face for rainbow-indent-and-delimeters."
  :group 'rainbow-indent-and-delimeters)

(defface rainbow-indent-and-delimeters-face2
  '((t (:foreground "#B69457")))
  "Face for rainbow-indent-and-delimeters."
  :group 'rainbow-indent-and-delimeters)

(defface rainbow-indent-and-delimeters-face3
  '((t (:foreground "#7BA55D")))
  "Face for rainbow-indent-and-delimeters."
  :group 'rainbow-indent-and-delimeters)

(defface rainbow-indent-and-delimeters-face4
  '((t (:foreground "#4F86A0")))
  "Face for rainbow-indent-and-delimeters."
  :group 'rainbow-indent-and-delimeters)

(defface rainbow-indent-and-delimeters-face5
  '((t (:foreground "#B55C2B")))
  "Face for rainbow-indent-and-delimeters."
  :group 'rainbow-indent-and-delimeters)

(defface rainbow-indent-and-delimeters-face6
  '((t (:foreground "#144BC3")))
  "Face for rainbow-indent-and-delimeters."
  :group 'rainbow-indent-and-delimeters)


(defun rainbow-indent-and-delimiters--highlight-function (level responsive display)
  "Highlight the current line with a face according to LEVEL.
RESPONSIVE and DISPLAY are ignored."
  (message "highlighting")
  (let* ((lvl (% level 6)))
    (cond
     ((eq lvl 0) 'rainbow-indent-and-delimeters-face1)
     ((eq lvl 1) 'rainbow-indent-and-delimeters-face2)
     ((eq lvl 2) 'rainbow-indent-and-delimeters-face3)
     ((eq lvl 3) 'rainbow-indent-and-delimeters-face4)
     ((eq lvl 4) 'rainbow-indent-and-delimeters-face5)
     ((eq lvl 5) 'rainbow-indent-and-delimeters-face6))))

(define-minor-mode rainbow-indent-and-delimiters-mode
  "Highlight the indentation level with a face."
  :lighter ""
  :global t
  (if rainbow-indent-and-delimiters-mode
      (progn
        (custom-set-faces
         '(rainbow-delimiters-depth-1-face ((t :inherit rainbow-indent-and-delimeters-face1)))
         '(rainbow-delimiters-depth-2-face ((t :inherit rainbow-indent-and-delimeters-face2)))
         '(rainbow-delimiters-depth-3-face ((t :inherit rainbow-indent-and-delimeters-face3)))
         '(rainbow-delimiters-depth-4-face ((t :inherit rainbow-indent-and-delimeters-face4)))
         '(rainbow-delimiters-depth-5-face ((t :inherit rainbow-indent-and-delimeters-face5)))
         '(rainbow-delimiters-depth-6-face ((t :inherit rainbow-indent-and-delimeters-face6))))
        (setq highlight-indent-guides-highlighter-function 'rainbow-indent-and-delimiters--highlight-function)
        (setq rainbow-delimiters-max-face-count 6)
        (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
        (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
        (add-hook 'lsp-mode-hook 'highlight-indent-guides-mode))
    (progn
      (custom-set-faces
       '(rainbow-delimiters-depth-1-face ((t :inherit rainbow-delimiters-depth-1-face)))
       '(rainbow-delimiters-depth-2-face ((t :inherit rainbow-delimiters-depth-2-face)))
       '(rainbow-delimiters-depth-3-face ((t :inherit rainbow-delimiters-depth-3-face)))
       '(rainbow-delimiters-depth-4-face ((t :inherit rainbow-delimiters-depth-4-face)))
       '(rainbow-delimiters-depth-5-face ((t :inherit rainbow-delimiters-depth-5-face)))
       '(rainbow-delimiters-depth-5-face ((t :inherit rainbow-delimiters-depth-6-face))))
      (setq highlight-indent-guides-highlighter-function 'highlight-indent-guides--highlighter-default)
      (setq rainbow-delimiters-max-face-count 9)
      (remove-hook 'prog-mode-hook 'rainbow-delimiters-mode)
      (remove-hook 'prog-mode-hook 'highlight-indent-guides-mode)
      (remove-hook 'lsp-mode-hook 'highlight-indent-guides-mode))))

(provide 'rainbow-indent-and-delimeters)
