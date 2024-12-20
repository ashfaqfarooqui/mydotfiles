;; -*- lexical-binding: t; -*-

(setq user-full-name "Ashfaq Farooqui")
     (setq user-mail-address "ashfaq@ashfaqfarooqui.me")

(setq auth-sources '("~/.authinfo.gpg")
      auth-source-cache-expiry nil) ; default is 7200 (2h)

;; (after! org
;;   (use-package org-pretty-tags
;;   :config
;;    (setq org-pretty-tags-surrogate-strings
;;          `(("uni"        . ,(all-the-icons-faicon   "graduation-cap" :face 'all-the-icons-purple  :v-adjust 0.01))
;;            ("ucc"        . ,(all-the-icons-material "computer"       :face 'all-the-icons-silver  :v-adjust 0.01))
;;            ("assignment" . ,(all-the-icons-material "library_books"  :face 'all-the-icons-orange  :v-adjust 0.01))
;;            ("test"       . ,(all-the-icons-material "timer"          :face 'all-the-icons-red     :v-adjust 0.01))
;;            ("lecture"    . ,(all-the-icons-fileicon "keynote"        :face 'all-the-icons-orange  :v-adjust 0.01))
;;            ("email"      . ,(all-the-icons-faicon   "envelope"       :face 'all-the-icons-blue    :v-adjust 0.01))
;;            ("read"       . ,(all-the-icons-octicon  "book"           :face 'all-the-icons-lblue   :v-adjust 0.01))
;;            ("article"    . ,(all-the-icons-octicon  "file-text"      :face 'all-the-icons-yellow  :v-adjust 0.01))
;;            ("web"        . ,(all-the-icons-faicon   "globe"          :face 'all-the-icons-green   :v-adjust 0.01))
;;            ("info"       . ,(all-the-icons-faicon   "info-circle"    :face 'all-the-icons-blue    :v-adjust 0.01))
;;            ("issue"      . ,(all-the-icons-faicon   "bug"            :face 'all-the-icons-red     :v-adjust 0.01))
;;            ("someday"    . ,(all-the-icons-faicon   "calendar-o"     :face 'all-the-icons-cyan    :v-adjust 0.01))
;;            ("idea"       . ,(all-the-icons-octicon  "light-bulb"     :face 'all-the-icons-yellow  :v-adjust 0.01))
;;            ("emacs"      . ,(all-the-icons-fileicon "emacs"          :face 'all-the-icons-lpurple :v-adjust 0.01))))
;;    (org-pretty-tags-global-mode)))

(after! org-superstar
  (setq org-superstar-headline-bullets-list '("◉" "○" "✸" "✿" "✤" "✜" "◆" "▶")
        ;; org-superstar-headline-bullets-list '("Ⅰ" "Ⅱ" "Ⅲ" "Ⅳ" "Ⅴ" "Ⅵ" "Ⅶ" "Ⅷ" "Ⅸ" "Ⅹ")
        org-superstar-prettify-item-bullets t ))
(after! org
  (setq org-ellipsis " ▾ "
        org-priority-highest ?A
        org-priority-lowest ?E
        org-priority-faces
        '((?A . 'all-the-icons-red)
          (?B . 'all-the-icons-orange)
          (?C . 'all-the-icons-yellow)
          (?D . 'all-the-icons-green)
          (?E . 'all-the-icons-blue))))

(after! org
  (appendq! +ligatures-extra-symbols
            `(:checkbox      "☐"
              :pending       "◼"
              :checkedbox    "☑"
              :list_property "∷"
              :results       "🠶"
              :property      "☸"
              :properties    "⚙"
              :end           "∎"
              :options       "⌥"
              :title         "𝙏"
              :subtitle      "𝙩"
              :author        "𝘼"
              :date          "𝘿"
              :latex_header  "⇥"
              :latex_class   "🄲"
              :beamer_header "↠"
              :begin_quote   "❮"
              :end_quote     "❯"
              :begin_export  "⯮"
              :end_export    "⯬"
              :priority_a   ,(propertize "⚑" 'face 'all-the-icons-red)
              :priority_b   ,(propertize "⬆" 'face 'all-the-icons-orange)
              :priority_c   ,(propertize "■" 'face 'all-the-icons-yellow)
              :priority_d   ,(propertize "⬇" 'face 'all-the-icons-green)
              :priority_e   ,(propertize "❓" 'face 'all-the-icons-blue)
              :em_dash       "—"))
  (set-ligatures! 'org-mode
    :merge t
    :checkbox      "[ ]"
    :pending       "[-]"
    :checkedbox    "[X]"
    :list_property "::"
    :results       "#+results:"
    :property      "#+property:"
    :property      ":PROPERTIES:"
    :end           ":END:"
    :options       "#+options:"
    :title         "#+title:"
    :subtitle      "#+subtitle:"
    :author        "#+author:"
    :date          "#+date:"
    :latex_class   "#+latex_class:"
    :latex_header  "#+latex_header:"
    :beamer_header "#+beamer_header:"
    :begin_quote   "#+begin_quote"
    :end_quote     "#+end_quote"
    :begin_export  "#+begin_export"
    :end_export    "#+end_export"
    :priority_a    "[#A]"
    :priority_b    "[#B]"
    :priority_c    "[#C]"
    :priority_d    "[#D]"
    :priority_e    "[#E]"
    :em_dash       "---"))
(plist-put +ligatures-extra-symbols :name "⁍") ; or › could be good?

;(add-hook 'org-mode-hook 'org-fragtog-mode)

(use-package! rainbow-mode
  :after rainbow-delimiter
:init (rainbow-mode))

(after! nyan-mode
     :init
    (nyan-mode))

(setq display-line-numbers-type 'relative)

(setq evil-move-cursor-back nil)

(after! super-save
(super-save-mode 1)
(setq super-save-exclude '(".gpg"))
(setq super-save-auto-save-when-idle t)
)

(add-hook! org-mode :append
           #'visual-line-mode)

(add-hook! text-mode :append
           #'visual-line-mode)

(add-hook! latex-mode :append
           #'visual-line-mode)

(use-package! visual-fill-column
  :config
  (add-hook 'visual-line-mode-hook #'visual-fill-column-mode)
  (advice-add 'text-scale-adjust :after
              #'visual-fill-column-adjust)
  (setq visual-fill-column-width 100)
  (setq-default fill-column 100)
  (setq visual-fill-column-center-text t)
  )

(after! smartparens
  :config
  (map! :map smartparens-mode-map
        "C-M-f" #'sp-forward-sexp
        "C-M-b" #'sp-backward-sexp
        "C-M-u" #'sp-backward-up-sexp
        "C-M-d" #'sp-down-sexp
        "C-M-p" #'sp-backward-down-sexp
        "C-M-n" #'sp-up-sexp
        "C-M-s" #'sp-splice-sexp
        "C-)" #'sp-forward-slurp-sexp
        "C-}" #'sp-forward-barf-sexp
        "C-(" #'sp-backward-slurp-sexp
        "C-M-)" #'sp-backward-slurp-sexp
        "C-M-)" #'sp-backward-barf-sexp))

(after! org
  (defun dcaps-to-scaps ()
    "Convert word in DOuble CApitals to Single Capitals."
    (interactive)
    (and (= ?w (char-syntax (char-before)))
         (save-excursion
           (let ((end (point)))
             (and (if (called-interactively-p)
                      (skip-syntax-backward "w")
                    (= -3 (skip-syntax-backward "w")))
                  (let (case-fold-search)
                    (looking-at "\\b[[:upper:]]\\{2\\}[[:lower:]]"))
                  (capitalize-region (point) end))))))
  (add-hook 'post-self-insert-hook #'dcaps-to-scaps nil 'local)

  (define-minor-mode dubcaps-mode
    "Toggle `dubcaps-mode'.  Converts words in DOuble CApitals to
Single Capitals as you type."
    :init-value nil
    :lighter (" DC")
    (if dubcaps-mode
        (add-hook 'post-self-insert-hook #'dcaps-to-scaps nil 'local)
      (remove-hook 'post-self-insert-hook #'dcaps-to-scaps 'local)))


  (add-hook 'text-mode-hook #'dubcaps-mode)
  (add-hook 'org-mode-hook #'dubcaps-mode))

(use-package! info-colors
  :defer t
  :commands (info-colors-fontify-node))

(add-hook 'Info-selection-hook 'info-colors-fontify-node)

                                        ;(add-hook 'Info-mode-hook #'mixed-pitch-mode)

(after! text-mode
  (add-hook! 'text-mode-hook
             ;; Apply ANSI color codes
             (with-silent-modifications
               (ansi-color-apply-on-region (point-min) (point-max)))))

(setq ispell-dictionary "en")

(defun greedily-do-daemon-setup ()
  (require 'org)
  (when (require 'mu4e nil t)
    (setq mu4e-confirm-quit t)
    (setq +mu4e-lock-greedy t)
    (setq +mu4e-lock-relaxed t)
    (+mu4e-lock-add-watcher)
    (when (+mu4e-lock-available t)
      (mu4e~start)))
  (when (require 'elfeed nil t)
    (run-at-time nil (* 8 60 60) #'elfeed-update)))

(when (daemonp)
  (add-hook 'emacs-startup-hook #'greedily-do-daemon-setup)
(add-hook! 'server-after-make-frame-hook (switch-to-buffer +doom-dashboard-name))
                        )

(delete-selection-mode 1)                         ; Replace selection when inserting text
(display-time-mode 1)                             ; Enable time in the mode-line
(display-battery-mode 1)                          ; On laptops it's nice to know how much power you have
(global-subword-mode 1)                           ; Iterate through CamelCase words
(setq initial-major-mode 'org-mode)
(setq hungry-delete-mode t)
(show-smartparens-mode)
(global-hungry-delete-mode)
(nyan-mode)

(defun doom-modeline-conditional-buffer-encoding ()
  "We expect the encoding to be LF UTF-8, so only show the modeline when this is not the case"
  (setq-local doom-modeline-buffer-encoding
              (unless (or (eq buffer-file-coding-system 'utf-8-unix)
                          (eq buffer-file-coding-system 'utf-8)))))

(add-hook 'after-change-major-mode-hook #'doom-modeline-conditional-buffer-encoding)

(after! org
(setq org-crypt-disable-auto-save nil)
(require 'org-crypt)
; Encrypt all entries before saving
(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance (quote ("crypt")))
; GPG key to use for encryption
(setq org-crypt-key "51DE2D88")
)

(setq org-download-screenshot-method "scrot -s %s")

;;;###autoload


(after! org
(defmacro unpackaged/def-org-maybe-surround (&rest keys)
  "Define and bind interactive commands for each of KEYS that surround the region or insert text.
Commands are bound in `org-mode-map' to each of KEYS.  If the
region is active, commands surround it with the key character,
otherwise call `org-self-insert-command'."
  `(progn
     ,@(cl-loop for key in keys
                for name = (intern (concat "unpackaged/org-maybe-surround-" key))
                for docstring = (format "If region is active, surround it with \"%s\", otherwise call `org-self-insert-command'." key)
                collect `(defun ,name ()
                           ,docstring
                           (interactive)
                           (if (region-active-p)
                               (let ((beg (region-beginning))
                                     (end (region-end)))
                                 (save-excursion
                                   (goto-char end)
                                   (insert ,key)
                                   (goto-char beg)
                                   (insert ,key)))
                             (call-interactively #'org-self-insert-command)))
                collect `(define-key org-mode-map (kbd ,key) #',name))))

(unpackaged/def-org-maybe-surround "~" "=" "*" "/" "+"))

;(setq org-attach-id-dir "./.attach")

(after! org (setq org-export-headline-levels 5)) ; I like nesting

(after! org
  (require 'ox-extra)
  (ox-extras-activate '(ignore-headlines)))

(defun markdown-convert-buffer-to-org ()
    "Convert the current buffer's content from markdown to orgmode format and save it with the current buffer's file name but with .org extension."
    (interactive)
    (shell-command-on-region (point-min) (point-max)
                             (format "pandoc -f markdown -t org -o %s"
                                     (concat (file-name-sans-extension (buffer-file-name)) ".org"))))

(after! treemacs
  (defvar treemacs-file-ignore-extensions '()
    "File extension which `treemacs-ignore-filter' will ensure are ignored")
  (defvar treemacs-file-ignore-globs '()
    "Globs which will are transformed to `treemacs-file-ignore-regexps' which `treemacs-ignore-filter' will ensure are ignored")
  (defvar treemacs-file-ignore-regexps '()
    "RegExps to be tested to ignore files, generated from `treeemacs-file-ignore-globs'")
  (defun treemacs-file-ignore-generate-regexps ()
    "Generate `treemacs-file-ignore-regexps' from `treemacs-file-ignore-globs'"
    (setq treemacs-file-ignore-regexps (mapcar 'dired-glob-regexp treemacs-file-ignore-globs)))
  (if (equal treemacs-file-ignore-globs '()) nil (treemacs-file-ignore-generate-regexps))
  (defun treemacs-ignore-filter (file full-path)
    "Ignore files specified by `treemacs-file-ignore-extensions', and `treemacs-file-ignore-regexps'"
    (or (member (file-name-extension file) treemacs-file-ignore-extensions)
        (let ((ignore-file nil))
          (dolist (regexp treemacs-file-ignore-regexps ignore-file)
            (setq ignore-file (or ignore-file (if (string-match-p regexp full-path) t nil)))))))
  (add-to-list 'treemacs-ignored-file-predicates #'treemacs-ignore-filter))

        (setq treemacs-file-ignore-extensions
      '(;; LaTeX
        "aux"
        "ptc"
        "fdb_latexmk"
        "fls"
        "synctex.gz"
        "toc"
        ;; LaTeX - glossary
        "glg"
        "glo"
        "gls"
        "glsdefs"
        "ist"
        "acn"
        "acr"
        "alg"
        ;; LaTeX - pgfplots
        "mw"
        ;; LaTeX - pdfx
        "pdfa.xmpi"
        ))
(setq treemacs-file-ignore-globs
      '(;; LaTeX
        "*/_minted-*"
        ;; AucTeX
        "*/.auctex-auto"
        "*/_region_.log"
        "*/_region_.tex"))

;;; :tools magit

(after! magit

 (setq       magit-save-repository-buffers nil
      ;; Don't restore the wconf after quitting magit, it's jarring
      magit-inhibit-save-previous-winconf t
      transient-values '((magit-commit "--gpg-sign=7A804BCB51DE2D88")
                         (magit-rebase "--autosquash" "--gpg-sign=7A804BCB51DE2D88")
                         (magit-pull "--rebase" "--gpg-sign=7A804BCB51DE2D88")))

 (setq magit-repolist-columns
      '(("Name"    25 magit-repolist-column-ident                  ())
        ("Version" 25 magit-repolist-column-version                ())
        ("D"        1 magit-repolist-column-dirty                  ())
        ("Branch"  10 magit-repolist-column-branch                () )
        ("L<U"      3 magit-repolist-column-unpulled-from-upstream ((:right-align t)))
        ("L>U"      3 magit-repolist-column-unpushed-to-upstream   ((:right-align t)))
        ("Path"    99 magit-repolist-column-path                   ())))


)

(add-to-list 'load-path "/usr/share/emacs/site-lisp/elpa-src/mu4e-1.10.8")
(after! mu4e
  (setq mu4e-maildir-shortcuts
        '( ("/ri.se/Inbox"               . ?i)
           ("/ashfaqfarooqui.me/Inbox"   . ?p)))

  (setq mu4e-headers-fields
        '((:flags . 6)
          (:account-stripe . 2)
          (:from-or-to . 25)
          (:folder . 10)
          (:recipnum . 2)
          (:subject . 80)
          (:human-date . 8))
        +mu4e-min-header-frame-width 142
        mu4e-headers-date-format "%d/%m/%y"
        mu4e-headers-time-format "⧖ %H:%M"
        mu4e-headers-results-limit 1000
        mu4e-index-cleanup t)

  (add-to-list 'mu4e-bookmarks
               '(:name "Yesterday's messages" :query "date:2d..1d" :key ?y) t)

  (defvar +mu4e-header--folder-colors nil)
  (appendq! mu4e-header-info-custom
            '((:folder .
               (:name "Folder" :shortname "Folder" :help "Lowest level folder" :function
                (lambda (msg)
                  (+mu4e-colorize-str
                   (replace-regexp-in-string "\\`.*/" "" (mu4e-message-field msg :maildir))
                   '+mu4e-header--folder-colors))))))


  ;; spell check
  (add-hook 'mu4e-compose-mode-hook 'flyspell-mode)
  (setq mu4e-update-interval 600)



                                        ;(setq mu4e-compose-signature-auto-include t)




  (setq mu4e-enable-mode-line t)

  (set-email-account! "ashfaqfarooqui.me"
                      '(
                        ( user-mail-address      . "ashfaq@ashfaqfarooqui.me"  )
                        ( user-full-name         . "Ashfaq Farooqui" )
                        (mu4e-sent-folder       . "/ashfaqfarooqui.me/Sent")
                        (mu4e-drafts-folder     . "/ashfaqfarooqui.me/Drafts")
                        (mu4e-trash-folder      . "/ashfaqfarooqui.me/Trash")
                        (mu4e-refile-folder     . "/ashfaqfarooqui.me/Archive")
                        (smtpmail-smtp-user     . "ashfaq.farooqui@mailbox.org")
                        ;;    (user-mail-address      . "ashfaq@ashfaqfarooqui.me")    ;; only needed for mu < 1.4
                        (mu4e-attachment-dir . "~/Documents/MailAttachments/Personal")
                        (smtpmail-smtp-server . "smtp.mailbox.org")
                        (smtpmail-stream-type . ssl )
                        (smtpmail-smtp-service . 465)
                        (mu4e-compose-signature . "---\nAshfaq Farooqui"))
                      t)
  (set-email-account! "ri.se"
                      '(
                        ( user-mail-address      . "ashfaq.farooqui@ri.se"  )
                        ( user-full-name         . "Ashfaq Farooqui" )
                        (mu4e-sent-folder       . "/ri.se/Sent Mail")
                        (mu4e-drafts-folder     . "/ri.se/Drafts")
                        (mu4e-trash-folder      . "/ri.se/Trash")
                        (mu4e-refile-folder     . "/ri.se/Arkiv")
                        (smtpmail-smtp-user     . "ashfaq.farooqui@ri.se")
                        ;;    (user-mail-address      . "ashfaq@ashfaqfarooqui.me")    ;; only needed for mu < 1.4
                        (mu4e-attachment-dir . "~/Documents/MailAttachments/ri.se")
                        (mu4e-compose-signature . "//Ashfaq Farooqui")
                        ( smtpmail-smtp-server   . "localhost" )
                        (smtpmail-stream-type . nil )
                        ( smtpmail-smtp-service . 1025)
                        )
                      t)

                                        ;(setq smtpmail-debug-verb t)


                                        ;(setq mu4e-compose-signature message-signature)


  )

;;;Taking the below from [[http://mbork.pl/2016-02-06_An_attachment_reminder_in_mu4e]]
(after! mu4e
    (defun mbork/message-attachment-present-p ()
      "Return t if an attachment is found in the current message."
      (save-excursion
        (save-restriction
          (widen)
          (goto-char (point-min))
          (when (search-forward "<#part" nil t) t))))

    (defcustom mbork/message-attachment-intent-re
      (regexp-opt '("I attach"
                    "I have attached"
                    "I've attached"
                    "I have included"
                    "I've included"
                    "see the attached"
                    "see the attachment"
                    "attached file"))
      "A regex which - if found in the message, and if there is no
    attachment - should launch the no-attachment warning.")

    (defcustom mbork/message-attachment-reminder
      "Are you sure you want to send this message without any attachment? "
      "The default question asked when trying to send a message
    containing `mbork/message-attachment-intent-re' without an
    actual attachment.")

    (defun mbork/message-warn-if-no-attachments ()
      "Ask the user if s?he wants to send the message even though
    there are no attachments."
      (when (and (save-excursion
                   (save-restriction
                     (widen)
                     (goto-char (point-min))
                     (re-search-forward mbork/message-attachment-intent-re nil t)))
                 (not (mbork/message-attachment-present-p)))
        (unless (y-or-n-p mbork/message-attachment-reminder)
          (keyboard-quit))))

    (add-hook 'message-send-hook #'mbork/message-warn-if-no-attachments)


)

(after! org-msg
                                        ;use-package! org-msg
                                        ;  :after mu4e
                                        ;:config
  (setq org-msg-options "html-postamble:nil H:5 num:nil ^:{} toc:nil"
	org-msg-startup "hidestars indent inlineimages"
	org-msg-greeting-fmt "\nHi *%s*,\n\n"
	org-msg-greeting-name-limit 3
	org-msg-signature "



 #+begin_signature
 //Ashfaq
 #+end_signature")
  )
