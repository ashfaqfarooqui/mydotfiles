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

(require 'helm-config)
(global-set-key (kbd "M-x") 'helm-M-x)
(helm-mode 1)
(use-package ibuffer-vc
:ensure t)
(defalias 'list-buffers 'ibuffer-other-window)

(ido-mode t)
(setq ido-enable-flex-matching t
      ido-use-virtual-buffers t)

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

(require 'neotree)
      (global-set-key [f8] 'neotree-toggle)
    (setq neo-smart-open t)
  (defun neotree-project-dir ()
    "Open NeoTree using the git root."
    (interactive)
    (let ((project-dir (projectile-project-root))
          (file-name (buffer-file-name)))
      (neotree-toggle)
      (if project-dir
          (if (neo-global--window-exists-p)
              (progn
                (neotree-dir project-dir)
                (neotree-find file-name)))
        (message "Could not find git project root."))))

 (global-set-key [f8] 'neotree-project-dir)

(setq neo-theme (if (display-graphic-p) 'icons 'arrow))

(use-package dired+
  :ensure t
  :config (require 'dired+)
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
  (add-hook 'prog-mode-hook
            '(lambda () (linum-mode 1))))

(require 'org-protocol)

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

(setq org-directory "~/Dropbox/orgs")

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

(require 'ob)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((sh . t)
   (dot . t)
   (ruby . t)
   (js . t)
   (C . t)
   (ledger .t)
   (scala . t)))

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
(elpy-use-ipython)

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

(use-package git-gutter+
  :ensure t
  :init
  (global-git-gutter+-mode)
  :config
  (setq git-gutter+-modified-sign "¤"))

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

(alert "Emacs has started")
