
(setq user-full-name "Ashfaq Farooqui")
(setq user-mail-address "ashfaq.farooqui@gmail.com")

(load "package")
    (package-initialize)
    (add-to-list 'package-archives
                 '("marmalade" . "http://marmalade-repo.org/packages/"))
    (add-to-list 'package-archives
                 '("melpa" . "http://melpa.milkbox.net/packages/") t)
    (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
    (add-to-list 'package-archives
                '("elpy" . "http://jorgenschaefer.github.io/packages/"))
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
    (setq package-archive-enable-alist '(("melpa" magit f)))
    (require 'cl)

(defvar ashfaq/packages '(ac-slime
 auto-complete
 graphviz-dot-mode
 magit
 org
 flycheck
 flyspell
 flyspell-correct
 flyspell-correct-helm
 helm-flyspell
 powerline
 helm
 helm-projectile
 ensime
 multiple-cursors
 find-file-in-repository
 yasnippet
 neotree
 rainbow-delimiters
 projectile
 evil
 org-bullets
 bind-key
 zenburn-theme
 writeroom-mode;;darkroom
 which-key
 elpy
 color-theme-sanityinc-tomorrow
 latex-preview-pane
 helm-mu
 pdf-tools
 org-ref
 auctex
 use-package
 auctex-latexmk
 writegood-mode
 )
 "Default packages")

(defun ashfaq/packages-installed-p ()
  (loop for pkg in ashfaq/packages
        when (not (package-installed-p pkg)) do (return nil)
        finally (return t)))

(unless (ashfaq/packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (dolist (pkg ashfaq/packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))

;  (require 'secrets "~/.emacs.d/secrets.el.gpg")

(defvar my/emacs-cache (concat user-emacs-directory ".cache/")
  "Folder to store cache files in.

Should end with a forward slash.")

(setq custom-file (concat my/emacs-cache "customize.el"))
(setq bookmark-default-file (concat my/emacs-cache "bookmarks"))
(setq recentf-save-file (concat my/emacs-cache "recentf"))

(use-package alert
  :ensure t
  :config
  (if (executable-find "notify-send")
      (setq alert-default-style 'libnotify)))

(setq inhibit-splash-screen t
      initial-scratch-message nil
      initial-major-mode 'org-mode)

(scroll-bar-mode 1)
(tool-bar-mode -1)
(menu-bar-mode 1)

(delete-selection-mode t)
(transient-mark-mode t)
(setq x-select-enable-clipboard t)

(setq-default indicate-empty-lines t)
(when (not indicate-empty-lines)
  (toggle-indicate-empty-lines))

(setq tab-width 4
      indent-tabs-mode nil)

(setq backup-by-copying t      ; don't clobber symlinks
      backup-directory-alist
      '(("." . "~/.emacs.d/backups/emacs-saves"))    ; don't litter my fs tree
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)       ; use versioned backups

(defalias 'yes-or-no-p 'y-or-n-p)

;; (global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-;") 'comment-or-uncomment-region)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-c C-k") 'compile)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "S-z") 'undo)
(global-set-key (kbd "M-g") 'goto-line) ; [Ctrl-l]
(global-set-key (kbd "C-L") 'recenter-top-bottom)
(global-set-key [f2] 'split-window-horizontally)
(global-set-key [f1] 'remove-split)
(global-set-key (kbd "C-x 9") 'split-window-horizontally)
(global-set-key (kbd "s--") 'split-window-vertically)
(global-set-key (kbd "s-+") 'remove-split)
(global-set-key (kbd "s-<up>") 'enlarge-window)
(global-set-key (kbd "s-<down>") 'shrink-window)
(global-set-key (kbd "s-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "s-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "M-k") 'kill-whole-line)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

(setq echo-keystrokes 0.1
          use-dialog-box nil
          visible-bell t)
    (show-paren-mode t)
  (global-hl-line-mode)
(blink-cursor-mode -1)


(defun dcaps-to-scaps ()
  "Convert word in DOuble CApitals to Single Capitals."
  (interactive)
  (and (= ?w (char-syntax (char-before)))
       (save-excursion
         (and (if (called-interactively-p)
                  (skip-syntax-backward "w")
                (= -3 (skip-syntax-backward "w")))
              (let (case-fold-search)
                (looking-at "\\b[[:upper:]]\\{2\\}[[:lower:]]"))
              (capitalize-word 1)))))

(define-minor-mode dubcaps-mode
  "Toggle `dubcaps-mode'.  Converts words in DOuble CApitals to
Single Capitals as you type."
  :init-value nil
  :lighter (" DC")
  (if dubcaps-mode
      (add-hook 'post-self-insert-hook #'dcaps-to-scaps nil 'local)
    (remove-hook 'post-self-insert-hook #'dcaps-to-scaps 'local)))

(add-hook 'text-mode-hook #'dubcaps-mode)

(use-package evil
  :ensure t
  :config
  (evil-mode 1)

  (use-package evil-leader
    :ensure t
    :config
    (global-evil-leader-mode))

  (use-package evil-surround
    :ensure t
    :config
    (global-evil-surround-mode))

  (use-package evil-indent-textobject
    :ensure t)
(progn
    (evil-ex-define-cmd "e[dit]" 'helm-find-files)
    (evil-ex-define-cmd "b[uffer]" 'helm-buffers-list)
    (bind-key "[escape]" 'keyboard-escape-quit evil-normal-state-map)
    (bind-key "[escape]" 'keyboard-escape-quit evil-visual-state-map)
    (bind-key "<escape>" 'keyboard-escape-quit)))

(setq helm-display-function 'helm-display-buffer-in-own-frame
        helm-display-buffer-reuse-frame t
        helm-use-undecorated-frame-option t)


(require 'helm-config)
(global-set-key (kbd "M-x") 'helm-M-x)
(helm-mode 1)
(use-package ibuffer-vc
:ensure t)
(defalias 'list-buffers 'ibuffer-other-window)

;   (ido-mode t)
 ;  (setq ido-enable-flex-matching t
 ;        ido-use-virtual-buffers t)

(setq column-number-mode t)

(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

(use-package autopair
     :ensure t)
;(require 'autopair)
(autopair-global-mode)

(add-to-list 'exec-path "/usr/local/bin")
(use-package company
  :ensure t
  :bind
  (:map company-active-map
        ("C-s" . company-search-candidates)
        ("<tab>" . company-complete-common-or-cycle)
        ("RET" . company-complete-selection)
        ("C-n" . company-select-next)
        ("C-p" . company-select-previous))
  :init
  (add-hook 'after-init-hook #'global-company-mode)
  :config
  (setq company-minimum-prefix-length 2)
  (setq company-idle-delay 0.5)
  (setq company-require-match nil)
  (use-package company-statistics
    :ensure t
    :config
    (setq company-statistics-file
          (concat my/emacs-cache "company-statistics-cache.el"))
    (add-hook 'company-mode-hook #'company-statistics-mode))
  (use-package company-math
    :ensure t
    :config
    (add-to-list 'company-backends 'company-math-symbols-latex))
  (use-package company-quickhelp
    :ensure t
    :config
    (company-quickhelp-mode))
  (use-package company-flx
    :ensure t
    :init
    (with-eval-after-load 'company
      (company-flx-mode +1)))
  (use-package company-web-html
    :ensure company-web)
  (use-package company-shell
    :ensure t
    :config
    (add-to-list 'company-backends 'company-shell)))

(add-hook 'after-init-hook 'global-company-mode)

(setq org-startup-indented t)
(use-package aggressive-indent
  :ensure t
  :config
  (global-aggressive-indent-mode)
  )

(defun untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer."
  (interactive)
  (indent-buffer)
  (untabify-buffer)
  (delete-trailing-whitespace))

(defun cleanup-region (beg end)
  "Remove tmux artifacts from region."
  (interactive "r")
  (dolist (re '("\\\\│\·*\n" "\W*│\·*"))
    (replace-regexp re "" nil beg end)))

(global-set-key (kbd "C-x M-t") 'cleanup-region)
(global-set-key (kbd "C-c n") 'cleanup-buffer)

(setq-default show-trailing-whitespace t)

(defun shift-region (distance)
  (let ((mark (mark)))
    (save-excursion
      (indent-rigidly (region-beginning) (region-end) distance)
      (push-mark mark t t)
      ;; Tell the command loop not to deactivate the mark
      ;; for transient mark mode
      (setq deactivate-mark nil))))

(defun shift-right ()
  (interactive)
  (shift-region 1))

(defun shift-left ()
  (interactive)
  (shift-region -1))

;; Bind (shift-right) and (shift-left) function to your favorite keys. I use
;; the following so that Ctrl-Shift-Right Arrow moves selected text one
;; column to the right, Ctrl-Shift-Left Arrow moves selected text one
;; column to the left:

(global-set-key [C-S-right] 'shift-right)
(global-set-key [C-S-left] 'shift-left)

(setq flyspell-issue-welcome-flag nil)
    (setq-default ispell-program-name "/usr/local/bin/aspell")
  (setq-default ispell-program-name "/usr/bin/aspell")
(setq-default ispell-list-command "list")

(require 'f)

(setq eshell-visual-commands
      '("less" "tmux" "htop" "top" "bash" "zsh" "fish"))

(setq eshell-visual-subcommands
      '(("git" "log" "l" "diff" "show")))

;; Prompt with a bit of help from http://www.emacswiki.org/emacs/EshellPrompt
(defmacro with-face (str &rest properties)
  `(propertize ,str 'face (list ,@properties)))

(defun eshell/abbr-pwd ()
  (let ((home (getenv "HOME"))
        (path (eshell/pwd)))
    (cond
     ((string-equal home path) "~")
     ((f-ancestor-of? home path) (concat "~/" (f-relative path home)))
     (path))))

(defun eshell/my-prompt ()
  (let ((header-bg "#161616"))
    (concat
;     (with-face user-login-name :foreground "#dc322f")
;     (with-face (concat "@" hostname) :foreground "#268bd2")
;     " "
     (with-face (eshell/abbr-pwd) :foreground "#008700")
     (if (= (user-uid) 0)
         (with-face "#" :foreground "red")
       (with-face "$" :foreground "#2345ba"))
     " ")))

(setq eshell-prompt-function 'eshell/my-prompt)
(setq eshell-highlight-prompt nil)
(setq eshell-prompt-regexp "^[^#$\n]+[#$] ")

(setq eshell-cmpl-cycle-completions nil)

(require 'powerline)
(powerline-default-theme)

(use-package treemacs
  :ensure t
  :defer t
  :config
  (progn
    (use-package treemacs-evil
      :ensure t
      :demand t)
    (setq treemacs-change-root-without-asking nil
          treemacs-collapse-dirs              (if (executable-find "python") 3 0)
          treemacs-file-event-delay           5000
          treemacs-follow-after-init          t
          treemacs-goto-tag-strategy          'refetch-index
          treemacs-indentation                2
          treemacs-indentation-string         " "
          treemacs-is-never-other-window      nil
          treemacs-never-persist              nil
          treemacs-no-png-images              nil
          treemacs-recenter-after-file-follow nil
          treemacs-recenter-after-tag-follow  nil
          treemacs-show-hidden-files          nil
          treemacs-silent-filewatch           nil
          treemacs-silent-refresh             nil
          treemacs-sorting                    'alphabetic-desc
          treemacs-tag-follow-cleanup         t
          treemacs-tag-follow-delay           1.5
          treemacs-width                      35)

    (with-eval-after-load 'winum
      (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null (executable-find "python3"))))
      (`(t . t)
       (treemacs-git-mode 'extended))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
        ([f8]         . treemacs-toggle)
        ("M-0"        . treemacs-select-window)
        ("C-c 1"      . treemacs-delete-other-windows)
        ("M-m ft"     . treemacs-toggle)
        ("M-m fT"     . treemacs)
        ("M-m fB"     . treemacs-bookmark)
        ("M-m f C-t"  . treemacs-find-file)
        ("M-m f M-t"  . treemacs-find-tag)))
(use-package treemacs-projectile
  :defer t
  :ensure t
  :config
  (setq treemacs-header-function #'treemacs-projectile-create-header)
  :bind (:map global-map
              ("M-m fP" . treemacs-projectile)
              ("M-m fp" . treemacs-projectile-toggle)))

(use-package winum
:ensure t
:config 
(winum-mode))

(use-package dired
  :ensure t
  :config (require 'dired)
  )

;;preview files in dired
(use-package peep-dired
  :ensure t
  :defer t ; don't access `dired-mode-map' until `peep-dired' is loaded
  :bind (:map dired-mode-map
              ("P" . peep-dired)))
(evil-define-key 'normal peep-dired-mode-map (kbd "<SPC>") 'peep-dired-scroll-page-down
                                             (kbd "C-<SPC>") 'peep-dired-scroll-page-up
                                             (kbd "<backspace>") 'peep-dired-scroll-page-up
                                             (kbd "j") 'peep-dired-next-file
                                             (kbd "k") 'peep-dired-prev-file)
(add-hook 'peep-dired-hook 'evil-normalize-keymaps)

(use-package volatile-highlights
:ensure t  
:config
  (volatile-highlights-mode t))

(use-package move-text
  :ensure t
  :bind
  (([(meta shift up)] . move-text-up)
   ([(meta shift down)] . move-text-down)))

(use-package linum
  :config
  (global-lineum-mode 1))

(require 'org-protocol)

(use-package counsel
  :ensure t
  )
(use-package ivy
  :ensure t
  )

(use-package swiper-helm
:ensure t
:config 
(progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (global-set-key "\C-s" 'swiper)
))

(use-package ace-window
          :ensure t
          :defer 1
          :config
          (set-face-attribute 'aw-leading-char-face nil :foreground "deep sky blue" :weight 'bold :height 3.0)
          (set-face-attribute 'aw-mode-line-face nil :inherit 'mode-line-buffer-id :foreground "lawn green")
          (setq aw-keys   '(?a ?s ?d ?f ?j ?k ?l)
                aw-dispatch-always t
                aw-dispatch-alist
                '((?x aw-delete-window     "Ace - Delete Window")
                  (?c aw-swap-window       "Ace - Swap Window")
                  (?n aw-flip-window)
                  (?v aw-split-window-vert "Ace - Split Vert Window")
                  (?h aw-split-window-horz "Ace - Split Horz Window")
                  (?m delete-other-windows "Ace - Maximize Window")
                  (?g delete-other-windows)
                  (?b balance-windows)
                  (?u winner-undo)
                  (?r winner-redo)))

            (defhydra hydra-window-size (:color red)
              "Windows size"
              ("h" shrink-window-horizontally "shrink horizontal")
              ("j" shrink-window "shrink vertical")
              ("k" enlarge-window "enlarge vertical")
              ("l" enlarge-window-horizontally "enlarge horizontal"))
            (defhydra hydra-window-frame (:color red)
              "Frame"
              ("f" make-frame "new frame")
              ("x" delete-frame "delete frame"))
            (defhydra hydra-window-scroll (:color red)
              "Scroll other window"
              ("n" joe-scroll-other-window "scroll")
              ("p" joe-scroll-other-window-down "scroll down"))
            (add-to-list 'aw-dispatch-alist '(?w hydra-window-size/body) t)
            (add-to-list 'aw-dispatch-alist '(?o hydra-window-scroll/body) t)
            (add-to-list 'aw-dispatch-alist '(?\; hydra-window-frame/body) t)
          (ace-window-display-mode t))


(defhydra hydra-frame-window (:color red :hint nil)
  "
^Delete^                       ^Frame resize^             ^Window^                Window Size^^^^^^   ^Text^                         (__)
_0_: delete-frame              _g_: resize-frame-right    _t_: toggle               ^ ^ _k_ ^ ^        _K_                           (oo)
_1_: delete-other-frames       _H_: resize-frame-left     _e_: ace-swap-win         _h_ ^+^ _l_        ^+^                     /------\\/
_2_: make-frame                _F_: fullscreen            ^ ^                       ^ ^ _j_ ^ ^        _J_                    / |    ||
_d_: kill-and-delete-frame     _n_: new-frame-right       _w_: ace-delete-window    _b_alance^^^^      ^ ^                   *  /\\---/\\  ~~  C-x f ;
"
  ("0" delete-frame :exit t)
  ("1" delete-other-frames :exit t)
  ("2" make-frame  :exit t)
  ("b" balance-windows)
  ("d" kill-and-delete-frame :exit t)
  ("e" ace-swap-window)
  ("F" toggle-frame-fullscreen)   ;; is <f11>
  ("g" resize-frame-right :exit t)
  ("H" resize-frame-left :exit t)  ;; aw-dispatch-alist uses h, I rebind here so hjkl can be used for size
  ("n" new-frame-right :exit t)
  ;; ("r" reverse-windows)
  ("t" toggle-window-spilt)
  ("w" ace-delete-window :exit t)
  ("x" delete-frame :exit t)
  ("K" text-scale-decrease)
  ("J" text-scale-increase)
  ("h" shrink-window-horizontally)
  ("k" shrink-window)
  ("j" enlarge-window)
  ("l" enlarge-window-horizontally))
(global-set-key (kbd "C-c h w") 'hydra-frame-window)

(require 'ensime)
  ;; Start ensime mode whenever we open scala mode, e.g. open a .scala file
  (add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
  ;; Start ensime with Super-e
  (global-set-key (kbd "C-c C-c c") 'ensime)
  ;; Configuration for ensime
  (setq ensime-sem-high-faces
    '(
       (implicitConversion nil)
       (var . (:foreground "#ff2222"))
       (val . (:foreground "#dddddd"))
       (varField . (:foreground "#ff3333"))
       (valField . (:foreground "#dddddd"))
       (functionCall . (:foreground "#dc9157"))
       (param . (:foreground "#ffffff"))
       (object . (:foreground "#D884E3"))
       (class . (:foreground "green"))
       (trait . (:foreground "#009933"))
       (operator . (:foreground "#cc7832"))
       (object . (:foreground "#6897bb" :slant italic))
       (package . (:foreground "yellow"))
       (implicitConversion . (:underline (:style wave :color "blue")))
       (implicitParams . (:underline (:style wave :color "blue")))
       (deprecated . (:strike-through "#a9b7c6"))
       (implicitParams nil)
     )
    ensime-completion-style 'company
    ensime-sem-high-enabled-p nil ;; disable semantic highlighting
    ensime-tooltip-hints t ;; disable type-inspecting tooltips
    ensime-tooltip-type-hints t ;; disable typeinspecting tooltips
)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)
(setq ido-use-filename-at-point 'guess)
(setq ido-create-new-buffer 'always)
(setq ido-file-extensions-order '(".scala" ".org" ".txt" ".py" ".emacs" ".xml" ".el" ".ini" ".cfg" ".cnf"))

(defun search-to-brace ()
  "Jump to the next open brace"
  (interactive)
  (search-forward "{"))
(define-key global-map (kbd "M-s {") 'search-to-brace)

(defun search-to-prev-brace ()
    "Jump to the previous brace"
    (interactive)
    (search-backward "{"))
(define-key global-map (kbd "M-S {") 'search-to-prev-brace)

(defun search-to-close-brace ()
  "Jump to the next close brace"
  (interactive)
  (search-forward "}"))
(define-key global-map (kbd "M-s }") 'search-to-close-brace)

(defun search-to-prev-close-brace ()
  "Jump to the previous close brace"
  (interactive)
  (search-backward "}"))
(define-key global-map (kbd "M-S }") 'search-to-prev-brace)

(defun search-to-next-def ()
  "Jump to the next def"
  (interactive)
  (search-forward "def "))
(define-key global-map (kbd "M-s d") 'search-to-next-def)

(defun search-to-prev-def ()
  "Jump to the previous def"
  (interactive)
  (search-backward "def "))
(define-key global-map (kbd "M-S d") 'search-to-prev-def)

;; Save on focus-out
(defun save-all ()
  (interactive)
  (save-some-buffers t))
(add-hook 'focus-out-hook 'save-all)

(defun scala-mode-newline-comments ()
  "Custom newline appropriate for `scala-mode'."
  ;; shouldn't this be in a post-insert hook?
  (interactive)
  (newline-and-indent)
  (scala-indent:insert-asterisk-on-multiline-comment))

(bind-key "RET" 'scala-mode-newline-comments scala-mode-map)

(setq comment-start "/* "
          comment-end " */"
          comment-style 'multi-line
          comment-empty-lines t)

(add-hook 'scala-mode-hook
          (lambda ()
            (show-paren-mode)
;            (smartparens-mode)
            (yas-minor-mode)
            (git-gutter-mode)
;            (company-mode)
            (ensime-mode)
            (scala-mode:goto-start-of-code)))

(setq org-directory "~/Orgs")

(defun org-file-path (filename)
  "Return the absolute address of an org file, given its relative name."
  (concat (file-name-as-directory org-directory) filename))

(setq org-inbox-file
      (concat (org-file-path "inbox.org")))
(setq org-index-file (org-file-path "index.org"))
(setq org-archive-location
      (concat (org-file-path "archive.org") "::* From %s"))

(setq org-todo-keywords '((sequence
                   "TODO(t)"  ; next action
                   "STARTED(s)"
                   "WAITING(w@/!)"
                   "SOMEDAY(.)" "|" "DONE(x!)" "CANCELLED(c@)")
                  (sequence "TODELEGATE(-)" "DELEGATED(d)" "|" "COMPLETE(x)")
                   (sequence "IDEA"))
       org-todo-keyword-faces '(("IDEA" . (:foreground "green" :weight bold))
                                     ("STARTED" . (:foreground "blue" :weight bold))
                                     ("CANCELLED" . (:foreground "red" :weight book))
                                     ("SOMEDAY" . (:foreground "red" :weight book))
                                     ("WAITING" . (:foreground "yellow" :weight book))
                                     ("COMPLETE" . (:foreground "green" :weight bold))
                                     ("DONE" . (:foreground "green" :weight bold))))



                 (setq org-log-done t)
                    (add-hook 'org-mode-hook
                              (lambda ()
                                (flyspell-mode)))
                    (add-hook 'org-mode-hook
                              (lambda ()
                                (writegood-mode)))
  (add-hook 'LaTeX-mode-hook (lambda () (writegood-mode)))
(add-hook 'LaTeX-mode-hook (lambda () (flyspell-mode)))

(add-hook 'org-mode-hook
          (lambda ()
            (org-bullets-mode t)))

(setq org-ellipsis "⤵")

(setq org-src-fontify-natively t)

(setq org-src-tab-acts-natively t)

(setq org-src-window-setup 'current-window)

(add-hook 'org-capture-mode-hook 'evil-insert-state)

(setq org-pretty-entities          t ; UTF8 all the things!
      org-support-shift-select     t ; holding shift and moving point should select things
      org-M-RET-may-split-line     nil ; M-RET may never split a line
      org-enforce-todo-dependencies t ; can't finish parent before children
      org-enforce-todo-checkbox-dependencies t ; can't finish parent before children
      org-hide-emphasis-markers t ; make words italic or bold, hide / and *
      org-catch-invisible-edits 'error ; don't let me edit things I can't see
      org-startup-indented t) ; start with indentation setup
(setq org-startup-with-inline-images t) ; show inline images
(setq org-log-done t)
(setq org-goto-interface (quote outline-path-completion))
(use-package htmlize
  :ensure t)
(setq org-special-ctrl-a/e t)

(use-package ob-async
:ensure t)

(use-package ob-ipython
:ensure t)
     (require 'ob)

     (org-babel-do-load-languages
      'org-babel-load-languages
      '((sh . t)
        (dot . t)
        (ruby . t)
        (js . t)
        (C . t)
        (ledger .t)
        (scala . t)
(python . t)
(ipython . t)))

     (add-to-list 'org-src-lang-modes (quote ("dot". graphviz-dot)))
     (add-to-list 'org-babel-tangle-lang-exts '("clojure" . "clj"))

     (defvar org-babel-default-header-args:clojure
       '((:results . "silent") (:tangle . "yes")))

     (defun org-babel-execute:clojure (body params)
       (lisp-eval-string body)
       "Done!")

     (provide 'ob-clojure)

     (setq org-src-fontify-natively t
           org-confirm-babel-evaluate nil)

     (add-hook 'org-babel-after-execute-hook (lambda ()
                                               (condition-case nil
                                                   (org-display-inline-images)
                                                 (error nil)))
               'append)

(setq org-agenda-files (list org-directory))
(setq org-agenda-include-diary t)
(setq org-agenda-include-all-todo t)

(defun mark-done-and-archive ()
  "Mark the state of an org-mode item as DONE and archive it."
  (interactive)
  (org-todo "DONE")
  (org-archive-subtree))

(define-key global-map "\C-c\C-x\C-s" 'mark-done-and-archive)
(setq org-log-done 'time)

(setq org-capture-templates
             (quote ( ("a" "Appointment" entry (file  (org-file-path "gcal.org"))
          "* %?\n\n%^T\n\n:PROPERTIES:\n\n:END:\n\n")

  ("t" "todo work" entry (file+headline "Phd Notebook.org" "Tasks")
  "* TODO %a %?\nSCHEDULE: %(org-insert-time-stamp (org-read-date nil t \"+2d\"))")

               ("b" "Blog idea"
                entry
                (file (org-file-path "blog-ideas.org"))
                "* TODO %?\n")

               ("p" "Phd Notes"
                entry
                (file (org-file-path "Phd Notebook.org")))

               ("R" "Random Notes"
                entry
                (file org-index-file)
                "* %?\n"))))

;; (setq ledger-expense-completions
;;      (list
;;       "" ;; needed for first | for mapconcat
;;       "Income:Salary"
;;       "Assets:Savings" "Assets:Checking"
;;       "Expenses:Dining" "Expenses:ToIndia" "Expenses:Additional" "Expenses:Groceries" "Expenses:Rent" "Expenses:Outfit:Apparel" "Expenses:Outfit:Accessories" "Expenses:Goods" "Expenses:Electronics:Gadgets" "Expenses:Phone" "Expenses:Tools" "Expenses:Transport" "Expenses:Entertainment"
;;       "Liabilities:ChalmersCard"
;;       ))
;;
;;
;;   (setq capture-expense-template
;;         "%%(org-read-date) * %%^{What}
;;       %%^{Expenses%s}  %%^{Amount}
;;       %%^{Assets%s}")
;;
;;   (setq capture-income-template
;;         "%%(org-read-date) * Salary
;;       Assets:Checking  %%^{Amount}
;;       Income:Salary")
;;
;;   (setq capture-credit-template
;;         "%%(org-read-date) * %%^{What}
;;       %%^{Expense%s}  %%^{Amount}
;;       Liabilities:ChalmersCard")
;;
;;   (setq capture-transfer-template
;;         "%%(org-read-date) * %%^{What}
;;       %%^{Assets%s}  %%^{Amount}
;;       %%^{Assets%s}")
;;
;;defun return-capture-expense-template ()
;;   (let ((compstring
;;          (mapconcat 'identity ledger-expense-completions  "|" )))
;; (format capture-expense-template compstring compstring)))
;;
;;defun return-capture-credit-template ()
;;   (let ((compstring
;;          (mapconcat 'identity ledger-expense-completions  "|" )))
;; (format capture-credit-template compstring compstring))
;; )
;;defun return-capture-income-template ()
;;   (let ((compstring
;;          (mapconcat 'identity ledger-expense-completions  "|" )))
;; (format capture-income-template compstring compstring))
;; )
;;defun return-capture-transfer-template ()
;;   (let ((compstring
;;          (mapconcat 'identity ledger-expense-completions  "|" )))
;; (format capture-transfer-template compstring compstring))
;; )
;;
;;setq org-capture-templates
;;       (append '(("l" "Ledger entries")
;;                 ("ls" "Spending" plain
;;                 (file "~/Dropbox/orgs/Accounts/finances.ledger")
;;                 (function return-capture-expense-template)
;;                 :empty-lines-before 1
;;                 :empty-lines-after 1)
;;                 ("lc" "Credit" plain
;;                 (file "~/Dropbox/orgs/Accounts/finances.ledger")
;;                 (function return-capture-credit-template)
;;                 :empty-lines-before 1
;;                 :empty-lines-after 1)
;;                 ("li" "Income" plain
;;                 (file "~/Dropbox/orgs/Accounts/finances.ledger")
;;                 (function return-capture-income-template)
;;                 :empty-lines-before 1
;;                 :empty-lines-after 1)
;;                 ("lt" "Transfer" plain
;;                 (file "~/Dropbox/orgs/Accounts/finances.ledger")
;;                 (function return-capture-transfer-template)
;;                 :empty-lines-before 1
;;                 :empty-lines-after 1)
;;
;;org-capture-templates))
;;

(add-hook 'org-capture-mode-hook 'evil-insert-state)

(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cc" 'org-capture)

(require 'which-key)
(which-key-mode)
(which-key-setup-side-window-bottom)

(projectile-mode)

(require 'flycheck)

(flycheck-define-checker proselint
  "A linter for prose."
  :command ("proselint" source-inplace)
  :error-patterns
  ((warning line-start (file-name) ":" line ":" column ": "
            (id (one-or-more (not (any " "))))
            (message (one-or-more not-newline)
                     (zero-or-more "\n" (any " ") (one-or-more not-newline)))
            line-end))
  :modes (text-mode markdown-mode gfm-mode org-mode))

(add-to-list 'flycheck-checkers 'proselint)

(add-hook 'text-mode-hook #'flycheck-mode)
(add-hook 'org-mode-hook #'flycheck-mode)
(add-hook 'LaTeX-mode-hook #'flycheck-mode)

(require 'flyspell-correct-helm)
(define-key flyspell-mode-map (kbd "C-;") 'flyspell-correct-previous-word-generic)

(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'org-mode-hook 'turn-on-auto-fill)

;;    (require 'tex-mik)
     (setq TeX-auto-save t)
     (setq TeX-parse-self t)
     (setq-default TeX-master nil)
     (add-hook 'LaTeX-mode-hook 'visual-line-mode)
     (add-hook 'LaTeX-mode-hook 'flyspell-mode)
     (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
     (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
     (setq reftex-plug-into-AUCTeX t)
(require 'auctex-latexmk)
   (auctex-latexmk-setup)
    ; (require 'auto-complete-auctex)
   (setq auctex-latexmk-inherit-TeX-PDF-mode t)
  ;   (setq TeX-PDF-mode t)

(require 'dbus)

(defun un-urlify (fname-or-url)
  "A trivial function that replaces a prefix of file:/// with just /."
  (if (string= (substring fname-or-url 0 8) "file:///")
     (substring fname-or-url 7)
    fname-or-url))

(defun th-evince-sync (file linecol &rest ignored)
  (let* ((fname (un-urlify file))
         (buf (find-buffer-visiting fname))
         (line (car linecol))
         (col (cadr linecol)))
    (if (null buf)
        (message "[Synctex]: %s is not opened..." fname)
      (switch-to-buffer buf)
      (goto-line (car linecol))
      (unless (= col -1)
        (move-to-column col)))))

(defvar *dbus-evince-signal* nil)

(defun enable-evince-sync ()
  (require 'dbus)
  (when (and
         (eq window-system 'x)
         (fboundp 'dbus-register-signal))
    (unless *dbus-evince-signal*
      (setf *dbus-evince-signal*
            (dbus-register-signal
             :session nil "/org/gnome/evince/Window/0"
             "org.gnome.evince.Window" "SyncSource"
             'th-evince-sync)))))

(add-hook 'LaTeX-mode-hook 'enable-evince-sync)

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "markdown"))

(use-package langtool
:ensure t
:config 
(setq langtool-language-tool-jar "~/.emacs.d/vendor/langtool/languagetool-commandline.jar")
(setq langtool-default-language "en-US")
(setq langtool-java-classpath nil))

(use-package synosaurus
:ensure t
:config
(setq synosaurus-choose-method 'popup))

(use-package helm-dictionary
:ensure t)



(require 'color-theme-sanityinc-tomorrow)
(load-theme 'sanityinc-tomorrow-night t)

(elpy-enable)
(setq python-shell-interpreter "jupyter"
      python-shell-interpreter-args "console --simple-prompt")

(require 'doi-utils)
(require 'org-ref-wos)
(require 'org-ref-scopus)
(require 'org-ref-isbn)
(require 'org-ref-arxiv)
(require 'org-ref-sci-id)
(require 'x2bib)
(require 'org-ref-latex)
(require 'org-ref-pdf)
(require 'org-ref-url-utils)
(setq reftex-default-bibliography '("~/Library/MasterReferences.bib"))

;; see org-ref for use of these variables
(setq org-ref-bibliography-notes "~/Library/notes.org"
      org-ref-default-bibliography '("~/Library/MasterReferences.bib")
      org-ref-pdf-directory "~/Library/bibtex-pdfs/")

(defun my/org-ref-open-pdf-at-point ()
  "Open the pdf for bibtex key under point if it exists."
  (interactive)
  (let* ((results (org-ref-get-bibtex-key-and-file))
         (key (car results))
         (pdf-file (car (bibtex-completion-find-pdf key))))
    (if (file-exists-p pdf-file)
        (org-open-file pdf-file)
      (message "No PDF found for %s" key))))

(setq org-ref-open-pdf-function 'my/org-ref-open-pdf-at-point)

(use-package pdf-tools
   :pin manual ;; manually update
   :config
   ;; open pdfs scaled to fit page
   (setq-default pdf-view-display-size 'fit-page)
   ;; automatically annotate highlights
   (setq pdf-annot-activate-created-annotations t)
   ;; use normal isearch
   (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward))
(add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
  (add-to-list 'auto-mode-alist '("\\.pdf\\'" . pdf-tools-install))

(use-package ledger-mode
   :ensure t
   :init
   (setq ledger-clear-whole-transactions 1)
   :mode ("\\.ledger$" . ledger-mode)
 :init
 (defvar my/ledger-file
   (expand-file-name "~/Dropbox/orgs/Accounts/finances.ledger")
   "Where the ledger journal is kept.")
 (setq file-ledger "finances.ledger")
   :config
   (add-to-list 'evil-emacs-state-modes 'ledger-report-mode)
 (setq ledger-post-amount-alignment-column 70)
  (setq ledger-post-amount-alignment-at :decimal)
  ;; There is a correct way to write dates:
  ;; https://xkcd.com/1179/
  (setq ledger-use-iso-dates t)
(setq ledger-reports '(("on-hand" "ledger -f %(ledger-file) --color bal \"(Assets:Checking|Savings|Liabilities)\"")
                       ("bal" "ledger -f %(ledger-file) --color bal")
                       ("reg" "ledger -f %(ledger-file) --color reg")
                       ("payee" "ledger -f %(ledger-file) --color  reg @%(payee)")
                       ("account" "ledger -f %(ledger-file) --color reg %(account)")
                       ("budgeted" "ledger --unbudgeted --monthly register ^expenses -f %(ledger-file)")
                       ("unbudgeted" "ledger --budgeted --monthly register ^expenses -f %(ledger-file)") )))

   (setq org-capture-templates
              (append  '(("l" "Ledger entries")
                  ("li" "income" plain (file my/ledger-file)
                   "%(org-read-date) *  %^{From?}
    Assets:Checking        SEK %^{Amount}
    Income:Salary" :empty-lines 1 :immediate-finish t)
                  ("lt" "transfer" plain (file my/ledger-file)
                   "%(org-read-date) %^{Payee}
    Assets:%^{account}        SEK %^{Amount}
    Assets:%^{account}" :empty-lines 1 :immediate-finish t)
                  ("lp" "Payment" plain (file my/ledger-file)
                   "%(org-read-date) %^{Payee}
    Expenses:%^{Expense category}        SEK %^{Amount}
    Assets:Checking" :empty-lines 1 :immediate-finish t)
                  ("ls" "pay from savings" plain (file my/ledger-file)
                   "%(org-read-date) %^{Payee}
    Expenses:%^{Expense category}        SEK %^{Amount}
    Assets:Savings" :empty-lines 1 :immediate-finish t)
                   ("lc" "pay with chalmers card" plain (file my/ledger-file)
                    "%(org-read-date) %^{Payee}
    Expenses:%^{Expense Category}       SEK %^{Amount}
    Liabilities:ChalmersCard" :empty-lines 1 :immediate-finish t ))
        org-capture-templates))

 (use-package flycheck-ledger
   :ensure t
   :init
   :mode "\\.ledger$'")

(use-package windmove
  ;; :defer 4
  :ensure t
  :config
 (global-set-key (kbd "C-c <left>")  'windmove-left)
 (global-set-key (kbd "C-c <right>") 'windmove-right)
 (global-set-key (kbd "C-c <up>")    'windmove-up)
 (global-set-key (kbd "C-c <down>")  'windmove-down)

  ;; wrap around at edges
  (setq windmove-wrap-around t))

;;use-package org-gcal
;; :ensure t
;; :config
;; (setq
;;       org-gcal-file-alist '(("ashfaq.farooqui@gmail.com" .  "~/Dropbox/orgs/gcal.org"))))
;;add-hook 'org-agenda-mode-hook (lambda () (org-gcal-sync) ))
;;add-hook 'org-capture-after-finalize-hook (lambda () (org-gcal-sync) ))
;;use-package calfw
;; :ensure  t;TODO:
;; :config
;; (require 'calfw)
;; (require 'calfw-org)
;; (setq cfw:org-overwrite-default-keybinding t)
;; (require 'calfw-ical)
;;
;; (defun mycalendar ()
;;   (interactive)
;;   (cfw:open-calendar-buffer
;;    :contents-sources
;;    (list
;;     ;; (cfw:org-create-source "Green")  ; orgmode source
;;     (cfw:ical-create-source "gcal" "https://calendar.google.com/calendar/ical/ashfaq.farooqui%40gmail.com/public/basic.ics" "IndianRed") ; google calendar ICS
;;     )))
;; (setq cfw:org-overwrite-default-keybinding t))
;;
;;use-package calfw-gcal
;;       :ensure t
;;       :config
;;       (require 'calfw-gcal))

;; 
;; (use-package org-caldav
;;   :ensure t
;;   :pin melpa
;;   :config
;;   (setq org-icalendar-timezone "Europe/Helsinki"
;;               org-caldav-debug-level 2 ; for debugging
;;         )
;;   (setq org-caldav-calendars
;;         '(
;;           (:calendar-id "calendar"
;;                         :url         "http://localhost:1080/users/ashfaqf@chalmers.se"
;;                         :inbox       "~/Orgs/calender.org"
;;                         :uuid-extension ".EML"
;;                         :files       ("~/Orgs/calender.org")
;;                         )
;;           ))
;;   )

;(add-to-list 'load-path "~/.emacs.d/dotEmacs/mu4e")
    (require 'mu4e)
    (require 'mu4e-contrib)
    (require 'helm-mu)
    (require 'org-mu4e)
    ;; default
    (setq mu4e-maildir "~/mail")
    (setq mu4e-drafts-folder "/Drafts")
    (setq mu4e-sent-folder   "/Sent")
    (setq mu4e-trash-folder  "/Trash")
      (setq mu4e-compose-signature-auto-include t)
  (setq mu4e-compose-format-flowed t)
    ;; don't save message to Sent Messages, Gmail/IMAP takes care of this
    (setq mu4e-sent-messages-behavior 'sent)
(setq mu4e-change-filenames-when-moving t)
    ;; setup some handy shortcuts
    ;; you can quickly switch to your Inbox -- press ``ji''
    ;; then, when you want archive some messages, move them to
    ;; the 'All Mail' folder by pressing ``ma''.

    (setq mu4e-maildir-shortcuts
          '( ("/Inbox"               . ?i)
             ("/Sent"   . ?s)
             ("/Drafts" . ?d)
             ("/Trash"       . ?t)
             ("/All Mail"    . ?a)))

    ;; allow for updating mail using 'U' in the main view:
    (setq mu4e-update-interval 600)
    (setq mu4e-get-mail-command "mbsync chalmers")
 ;; disable auto-save for email: since I have set
 ;; auto-save-visited-file-name, auto-save seems to leave multiple copies of
 ;; a message in the drafts folder. this is not nice.
 (add-hook 'mu4e-compose-mode-hook #'(lambda () (auto-save-mode -1))) 
    ;; something about ourselves
    (setq
     user-mail-address "ashfaqf@chalmers.se"
     user-full-name  "Ashfaq Farooqui"
     message-signature
     (concat
      "//Ashfaq"
      "\n"))
    (setq mu4e-compose-signature t)
    ;; sending mail -- replace USERNAME with your gmail username
    ;; also, make sure the gnutls command line utils are installed
    ;; package 'gnutls-bin' in Debian/Ubuntu

    (require 'smtpmail)
    (setq message-send-mail-function 'smtpmail-send-it
        smtpmail-auth-credentials
      (expand-file-name "~/.emacs.d/.authinfo.gpg")
        smtpmail-default-smtp-server "localhost"
        smtpmail-smtp-server "localhost"
        smtpmail-smtp-service 1025)

    ;; don't keep message buffers around
    (setq message-kill-buffer-on-exit t)
    ;;store org-mode links to messages
    ;;store link to message if in header view, not to header query
    (setq org-mu4e-link-query-in-headers-mode nil)



    ;;; Html rendering
    (setq mu4e-view-prefer-html t)

    (setq mu4e-use-fancy-chars t)
    (setq mu4e-attachment-dir "~/Documents/mail")


    ;;; Attempt to show images when viewing messages
    (setq mu4e-view-show-images t
          mu4e-view-image-max-width 800)

    ;; View html message in firefox (type aV)
    (add-to-list 'mu4e-view-actions
                '("ViewInBrowser" . mu4e-action-view-in-browser) t)

  ;; PGP-Sign all e-mails
  (add-hook 'message-send-hook 'mml-secure-message-sign-pgpmime)

(use-package helm-spotify-plus
:ensure t)

(use-package google-translate
  :ensure t
  :bind
  (:map mu4e-view-mode-map
        ("C-c t" . google-translate-at-point))
  :config
  (setq google-translate-default-target-language "English")
  ;; It won't ask for the input language. If I need it to, call the
  ;; translation command with a C-u prefix:
  (setq google-translate-default-source-language "Swedish"))

(use-package csv-mode
  :ensure t
  :mode (("\\.csv" . csv-mode)))

(use-package elfeed
  :ensure t
  :bind
  (:map elfeed-search-mode-map
        ("s" . bjm/elfeed-load-db-and-open)
        ("q" . bjm/elfeed-save-db-and-bury))
  :init
  ;; thanks - http://pragmaticemacs.com/emacs/read-your-rss-feeds-in-emacs-with-elfeed/
  ;; though slightly modified
  ;; functions to support syncing .elfeed between machines
  ;; makes sure elfeed reads index from disk before launching
  (defun bjm/elfeed-load-db-and-open ()
    "Load the elfeed db from disk before opening."
    (interactive)
    (elfeed-db-load)
    (elfeed)
    (elfeed-search-update--force)
    (elfeed-update))
  ;;write to disk when quiting
  (defun bjm/elfeed-save-db-and-bury ()
    "Wrapper to save the elfeed db to disk before burying buffer"
    (interactive)
    (elfeed-db-save)
    (quit-window))
  :config
  (setq elfeed-db-directory "~/Dropbox/.elfeed")
  ;; This lets me get the http links to entries with org-capture
  ;; easily.
  (defun elfeed-entry-as-html-link ()
    "Store an http link to an elfeed entry"
    (when (equal major-mode 'elfeed-show-mode)
      (let ((description (elfeed-entry-title elfeed-show-entry))
            (link (elfeed-entry-link elfeed-show-entry)))
        (org-store-link-props
         :type "http"
         :link link
         :description description))))
  (org-link-set-parameters "elfeed" :follow #'browse-url :store #'elfeed-entry-as-html-link))
(evil-define-key* 'motion elfeed-search-mode-map
                  "U" #'elfeed-search-update--force
                  "u" #'elfeed-search-fetch)

(evil-define-key* 'motion elfeed-show-mode-map
                  "j" #'elfeed-show-next
                  "k" #'elfeed-show-prev)
(add-to-list 'evil-motion-state-modes 'elfeed-search-mode)
(add-to-list 'evil-motion-state-modes 'elfeed-show-mode)
(use-package elfeed-goodies
:ensure t)


(use-package elfeed-org
  :ensure t
  :config
  (progn
    (elfeed-org)
    (setq rmh-elfeed-org-files (list "~/.emacs.d/elfeed.org"))))

(use-package pocket-reader
  :ensure t)

(use-package git-gutter
  :ensure t
  :init
  (global-git-gutter-mode)
  :config
  (setq git-gutter-modified-sign "¤")
(git-gutter:linum-setup))

(use-package org-board
:ensure t
)

(use-package restart-emacs
:ensure t)

(use-package dashboard
    :ensure t
    :config
    (dashboard-setup-startup-hook)
;; Set the title
(setq dashboard-banner-logo-title "Welcome to Emacs Dashboard")
;; Set the banner
(setq dashboard-startup-banner 'official)
;; Value can be
;; 'official which displays the official emacs logo
;; 'logo which displays an alternative emacs logo
;; 1, 2 or 3 which displays one of the text banners
;; "path/to/your/image.png which displays whatever image you would prefer
(setq dashboard-items '((recents  . 5)
                        (bookmarks . 5)
                        (projects . 5)
                        (agenda . 2)
                        (registers . 2)))
)

(use-package perspective
:ensure t)

(use-package org-web-tools
:ensure t)

(use-package toc-org
:ensure t)
(add-hook 'org-mode-hook 'toc-org-enable)

(use-package jedi
:ensure t
:init
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:setup-keys t) ; optional
(setq jedi:complete-on-dot t) ; optional
)

(use-package ein
:ensure t
:config
)

(use-package ein
:ensure t
:init
(require 'ein)
(require 'ein-dev)
(require 'auto-complete)
(require 'jedi)
(setq ein:completion-backend 'ein:use-ac-jedi-backend)
(setq ein:complete-on-dot t)

:config
(highlight-indentation-mode t)
)

(use-package pomidor
:ensure t
:config 
((setq pomidor-sound-tick nil
      pomidor-sound-tack nil
      pomidor-sound-overwork nil))
 (global-set-key (kbd "<f12>") #'pomidor)
)

(use-package biblio
:ensure t)

(use-package bibliothek
:ensure t
:config 
(setq bibliothek-path (list "/home/ashfaqf/readingMaterial"))
)

(defhydra hydra-projectile-other-window (:color teal)
  "projectile-other-window"
  ("f"  projectile-find-file-other-window        "file")
  ("g"  projectile-find-file-dwim-other-window   "file dwim")
  ("d"  projectile-find-dir-other-window         "dir")
  ("b"  projectile-switch-to-buffer-other-window "buffer")
  ("q"  nil                                      "cancel" :color blue))

(defhydra hydra-projectile (:color teal
                            :hint nil)
  "
     PROJECTILE: %(projectile-project-root)

     Find File            Search/Tags          Buffers                Cache
------------------------------------------------------------------------------------------
_s-f_: file            _a_: ag                _i_: Ibuffer           _c_: cache clear
 _ff_: file dwim       _g_: update gtags      _b_: switch to buffer  _x_: remove known project
 _fd_: file curr dir   _o_: multi-occur     _s-k_: Kill all buffers  _X_: cleanup non-existing
  _r_: recent file                                               ^^^^_z_: cache current
  _d_: dir

"
  ("a"   projectile-ag)
  ("b"   projectile-switch-to-buffer)
  ("c"   projectile-invalidate-cache)
  ("d"   projectile-find-dir)
  ("s-f" projectile-find-file)
  ("ff"  projectile-find-file-dwim)
  ("fd"  projectile-find-file-in-directory)
  ("g"   ggtags-update-tags)
  ("s-g" ggtags-update-tags)
  ("i"   projectile-ibuffer)
  ("K"   projectile-kill-buffers)
  ("s-k" projectile-kill-buffers)
  ("m"   projectile-multi-occur)
  ("o"   projectile-multi-occur)
  ("s-p" projectile-switch-project "switch project")
  ("p"   projectile-switch-project)
  ("s"   projectile-switch-project)
  ("r"   projectile-recentf)
  ("x"   projectile-remove-known-project)
  ("X"   projectile-cleanup-known-projects)
  ("z"   projectile-cache-current-file)
  ("`"   hydra-projectile-other-window/body "other window")
  ("q"   nil "cancel" :color blue))
(global-set-key (kbd "C-c h p") 'hydra-projectile/body)

(defhydra hydra-git-gutter (:body-pre (git-gutter-mode 1)
                            :hint nil)
  "
Git gutter:
  _j_: next hunk        _s_tage hunk     _q_uit
  _k_: previous hunk    _r_evert hunk    _Q_uit and deactivate git-gutter
  ^ ^                   _p_opup hunk
  _h_: first hunk
  _l_: last hunk        set start _R_evision
"
  ("j" git-gutter:next-hunk)
  ("k" git-gutter:previous-hunk)
  ("h" (progn (goto-char (point-min))
              (git-gutter:next-hunk 1)))
  ("l" (progn (goto-char (point-min))
              (git-gutter:previous-hunk 1)))
  ("s" git-gutter:stage-hunk)
  ("r" git-gutter:revert-hunk)
  ("p" git-gutter:popup-hunk)
  ("R" git-gutter:set-start-revision)
  ("q" nil :color blue)
  ("Q" (progn (git-gutter-mode -1)
              ;; git-gutter-fringe doesn't seem to
              ;; clear the markup right away
              (sit-for 0.1)
              (git-gutter:clear))
       :color blue))
(global-set-key (kbd "C-c h g") 'hydra-git-gutter/body)

(defhydra hydra-global-org (:color blue)
  "Org"
  ("i" org-timer-start "Start Timer")
  ("o" org-timer-stop "Stop Timer")
  ("s" org-timer-set-timer "Set Timer") ; This one requires you be in an orgmode doc, as it sets the timer for the header
  ("p" org-timer "Print Timer") ; output timer value to buffer
  ("w" (org-clock-in '(4)) "Clock-In") ; used with (org-clock-persistence-insinuate) (setq org-clock-persist t)
  ("o" org-clock-out "Clock-Out") ; you might also want (setq org-log-note-clock-out t)
  ("j" org-clock-goto "Clock Goto") ; global visit the clocked task
  ("c" org-capture "Capture") ; Don't forget to define the captures you want http://orgmode.org/manual/Capture.html
  ("l" org-capture-goto-last-stored "Last Capture")
  ("r" org-clock-report)
  ("?" (org-info "Clocking commands")))


(global-set-key (kbd "C-c o") 'hydra-global-org/body)

(defhydra hydra-ibuffer-main (:color pink :hint nil)
  "
 ^Navigation^ | ^Mark^        | ^Actions^        | ^View^
-^----------^-+-^----^--------+-^-------^--------+-^----^-------
  _k_:    ʌ   | _m_: mark     | _D_: delete      | _g_: refresh
 _RET_: visit | _u_: unmark   | _S_: save        | _s_: sort
  _j_:    v   | _*_: specific | _a_: all actions | _/_: filter
-^----------^-+-^----^--------+-^-------^--------+-^----^-------
"
  ("j" ibuffer-forward-line)
  ("RET" ibuffer-visit-buffer :color blue)
  ("k" ibuffer-backward-line)

  ("m" ibuffer-mark-forward)
  ("u" ibuffer-unmark-forward)
  ("*" hydra-ibuffer-mark/body :color blue)

  ("D" ibuffer-do-delete)
  ("S" ibuffer-do-save)
  ("a" hydra-ibuffer-action/body :color blue)

  ("g" ibuffer-update)
  ("s" hydra-ibuffer-sort/body :color blue)
  ("/" hydra-ibuffer-filter/body :color blue)

  ("o" ibuffer-visit-buffer-other-window "other window" :color blue)
  ("q" quit-window "quit ibuffer" :color blue)
  ("." nil "toggle hydra" :color blue))

(defhydra hydra-ibuffer-mark (:color teal :columns 5
                              :after-exit (hydra-ibuffer-main/body))
  "Mark"
  ("*" ibuffer-unmark-all "unmark all")
  ("M" ibuffer-mark-by-mode "mode")
  ("m" ibuffer-mark-modified-buffers "modified")
  ("u" ibuffer-mark-unsaved-buffers "unsaved")
  ("s" ibuffer-mark-special-buffers "special")
  ("r" ibuffer-mark-read-only-buffers "read-only")
  ("/" ibuffer-mark-dired-buffers "dired")
  ("e" ibuffer-mark-dissociated-buffers "dissociated")
  ("h" ibuffer-mark-help-buffers "help")
  ("z" ibuffer-mark-compressed-file-buffers "compressed")
  ("b" hydra-ibuffer-main/body "back" :color blue))

(defhydra hydra-ibuffer-action (:color teal :columns 4
                                :after-exit
                                (if (eq major-mode 'ibuffer-mode)
                                    (hydra-ibuffer-main/body)))
  "Action"
  ("A" ibuffer-do-view "view")
  ("E" ibuffer-do-eval "eval")
  ("F" ibuffer-do-shell-command-file "shell-command-file")
  ("I" ibuffer-do-query-replace-regexp "query-replace-regexp")
  ("H" ibuffer-do-view-other-frame "view-other-frame")
  ("N" ibuffer-do-shell-command-pipe-replace "shell-cmd-pipe-replace")
  ("M" ibuffer-do-toggle-modified "toggle-modified")
  ("O" ibuffer-do-occur "occur")
  ("P" ibuffer-do-print "print")
  ("Q" ibuffer-do-query-replace "query-replace")
  ("R" ibuffer-do-rename-uniquely "rename-uniquely")
  ("T" ibuffer-do-toggle-read-only "toggle-read-only")
  ("U" ibuffer-do-replace-regexp "replace-regexp")
  ("V" ibuffer-do-revert "revert")
  ("W" ibuffer-do-view-and-eval "view-and-eval")
  ("X" ibuffer-do-shell-command-pipe "shell-command-pipe")
  ("b" nil "back"))

(defhydra hydra-ibuffer-sort (:color amaranth :columns 3)
  "Sort"
  ("i" ibuffer-invert-sorting "invert")
  ("a" ibuffer-do-sort-by-alphabetic "alphabetic")
  ("v" ibuffer-do-sort-by-recency "recently used")
  ("s" ibuffer-do-sort-by-size "size")
  ("f" ibuffer-do-sort-by-filename/process "filename")
  ("m" ibuffer-do-sort-by-major-mode "mode")
  ("b" hydra-ibuffer-main/body "back" :color blue))

(defhydra hydra-ibuffer-filter (:color amaranth :columns 4)
  "Filter"
  ("m" ibuffer-filter-by-used-mode "mode")
  ("M" ibuffer-filter-by-derived-mode "derived mode")
  ("n" ibuffer-filter-by-name "name")
  ("c" ibuffer-filter-by-content "content")
  ("e" ibuffer-filter-by-predicate "predicate")
  ("f" ibuffer-filter-by-filename "filename")
  (">" ibuffer-filter-by-size-gt "size")
  ("<" ibuffer-filter-by-size-lt "size")
  ("/" ibuffer-filter-disable "disable")
  ("b" hydra-ibuffer-main/body "back" :color blue))
(define-key ibuffer-mode-map "." 'hydra-ibuffer-main/body)

(add-hook 'ibuffer-hook #'hydra-ibuffer-main/body)

(defhydra hydra-multiple-cursors (:hint nil)
  "
     ^Up^            ^Down^        ^Other^
----------------------------------------------
[_p_]   Next    [_n_]   Next    [_l_] Edit lines
[_P_]   Skip    [_N_]   Skip    [_a_] Mark all
[_M-p_] Unmark  [_M-n_] Unmark  [_r_] Mark by regexp
^ ^             ^ ^             [_q_] Quit
"
  ("l" mc/edit-lines :exit t)
  ("a" mc/mark-all-like-this :exit t)
  ("n" mc/mark-next-like-this)
  ("N" mc/skip-to-next-like-this)
  ("M-n" mc/unmark-next-like-this)
  ("p" mc/mark-previous-like-this)
  ("P" mc/skip-to-previous-like-this)
  ("M-p" mc/unmark-previous-like-this)
  ("r" mc/mark-all-in-region-regexp :exit t)
  ("q" nil))

(global-set-key (kbd "C-c h m") 'hydra-multiple-cursors/body)

(alert "Emacs has started")
