;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
                                        ;(package! some-package)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/raxod502/straight.el#the-recipe-format
                                        ;(package! another-package
                                        ;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
                                        ;(package! this-package
                                        ;  :recipe (:host github :repo "username/repo"
                                        ;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
                                        ;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
                                        ;(package! builtin-package :recipe (:nonrecursive t))
                                        ;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see raxod502/straight.el#279)
                                        ;(package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
                                        ;(package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
                                        ;(unpin! pinned-package)
;; ...or multiple packages
                                        ;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
                                        ;(unpin! t)

(package! company-tabnine
  :recipe (:host github :repo "TommyX12/company-tabnine"))

(package! copilot
  :recipe (:host github :repo "zerolfx/copilot.el" :files ("*.el" "dist")))

(package! company-box
  :recipe (
           :host github
           :repo "sebastiencs/company-box"
           :files ("*.el" "images" )))
(package! emmet-mode)
;; (package! beacon
;;   :recipe (
;;            :host github :repo "Malabarba/beacon"
;;            :files ("*.el")))

(package! yascroll
  :recipe (
           :host github
           :repo "emacsorphanage/yascroll"
           :files ("*.el")))
(package! inheritenv)
(package! language-id)
(package! sidekick
  :recipe (
           :type git :host github :repo "VernonGrant/sidekick.el"
           :files ("*.el")
           :branch "main"
           ))
(package! verb)
(package! all-the-icons-completion)
(package! helm-swoop)
(package! systemd)
(package! lsp-ui)

(unpin! lsp-mode)
(package! vertico-posframe)
(package! ivy)
(package! ivy-posframe)
(package! persist)
(package! daemons)
(package! dired-hide-dotfiles)

(package! ejc-sql)

(package! async-completing-read
  :recipe (
           :type git :host github :repo "oantolin/async-completing-read"
           :files ("*.el")
           :branch "master"))

(package! lsp-tailwindcss :recipe (:host github :repo "merrickluo/lsp-tailwindcss"))
(package! smudge)
(package! pcap-mode
  :recipe (
           :host github :repo "orgcandman/pcap-mode"
           :files ("*.el")))

(package! sly)

;; (package! tsi :recipe (:host github :repo "orzechowskid/tsi.el" :branch "main"))
;; (package! coverlay)
;; (package! origami)
;; (package! rjsx-mode)
(package! blamer)

(package! dired-posframe)
(package! which-key-posframe)
(package! mini-frame)
;; (package! evil-owl)
(package! man-posframe
  :recipe ( :type git :host github :repo "Ghibranalj/man-posframe.el"
                  :files ("*.el") :branch "master"))

(package! prisma-mode
  :recipe (:host github :repo "pimeys/emacs-prisma-mode" :branch "main"))

(package! pnpm-mode)
