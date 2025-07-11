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

(package! hungry-delete)
(package! rainbow-mode)
                                        ;(package! org-chef)
                                        ;(package! try)
(package! super-save)
                                        ;(package! org-super-agenda)
;;(package! flyspell-correct-ivy)
(package! alert)
(package! catppuccin-theme)                                       ;(package! undo-tree)
                                        ;(package! company-lsp)
;;(package! company-box)
                                        ;(package! org-msg)
                                        ;(package! mu4e-conversation)

;(package! rg)
                                        ;(package! keycast)
                                        ;(package! org-bullets)
                                        ;(package! flycheck-ledger)
                                        ;(package! aggressive-indent)
(package! nyan-mode)
;;(package! rg)
;;(package! minions)
                                        ;(package! org-roam-server)
;;(package! deft)
;;(package! visual-fill-column)
                                        ;(package! htmlize)
                                        ;(package! modus-themes
                                        ;  :recipe (:host gitlab :repo "protesilaos/modus-themes"))
                                        ;(package! pretty-hydra)  ;; dependency
                                        ;(package! org-media-note :recipe (:host github :repo "yuchen-lea/org-media-note"))

;;(package! flyspell-lazy)
;;(package! elfeed-goodies)
                                        ;(package! lexic)
                                        ;(package! holy-books)
                                        ;(package! info-colors)
;; Testing the org fold branch
                                        ;(package! org-fragtog)
;;(package! org-pretty-tags)
                                        ;(package! gif-screencast)
                                        ;(package! request)
                                        ;(package! activity-watch-mode)

                                        ;(package! org-pretty-table-mode
                                        ;  :recipe (:host github :repo "Fuco1/org-pretty-table"))
                                        ;(package! org-ref)
                                        ;(package! org-roam-bibtex)
                                        ;(package! iedit)
                                        ;(package! ob-ammonite)
                                        ;(package! flycheck-grammarly)
                                        ;(package! mixed-pitch :recipe (:local-repo "lisp/mixed-pitch") :pin nil)
                                        ;(package! org-appear :recipe (:host github :repo "awth13/org-appear")) ;
                                        ;(package! websocket)
                                        ;(package! org-roam-ui :recipe (:host github :repo "org-roam/org-roam-ui" :files ("*.el" "out")))
                                        ;(unpin! org-roam)

                                        ;(package! elfeed-goodies :disable t)
                                        ;(package! org-transclusion
                                        ;  :recipe (:host github
                                        ;           :repo "nobiot/org-transclusion"
                                        ;           :branch "main"
                                        ;           :files ("*.el")))
                                        ;(package! org-web-tools)

;; $DOOMDIR/config.el
