;; -*- lexical-binding: t; -*-

(setq user-full-name "Ashfaq Farooqui")
     (setq user-mail-address "ashfaq@ashfaqfarooqui.me")

(setq auth-sources '("~/.authinfo.gpg")
      auth-source-cache-expiry nil) ; default is 7200 (2h)

(setq doom-font (font-spec :family "JetBrainsMono Nerd Font Mono" :size 20)
      doom-big-font (font-spec :family "JetBrainsMono Nerd Font Mono" :size 35)
      doom-variable-pitch-font (font-spec :family "JetBrainsMono Nerd Font Mono" :size 20))

                                        ;(setq doom-font (font-spec :family "Overpass" :size 30)
                                        ;  doom-big-font (font-spec :family "fira code retina" :size 50)
                                        ;doom-variable-pitch-font (font-spec :family "Overpass" :size 33))
(setq doom-theme 'catppuccin)



                                        ;(setq doom-font (font-spec :family "mononoki Nerd Font" :size 12 :weight 'semi-light)
                                        ;      doom-variable-pitch-font (font-spec :family "mononoki Nerd Font") ; inherits `doom-font''s :size
                                        ;      doom-big-font (font-spec :family "mononoki Nerd Font" :size 19))




                                        ;(setq doom-font (font-spec :family "mononoki Nerd Font" :size 12 :weight 'semi-light)
                                        ;      doom-variable-pitch-font (font-spec :family "Fira Sans") ; inherits `doom-font''s :size
                                        ;      doom-unicode-font (font-spec :family "mononoki Nerd Font" :size 12)
                                        ;      doom-big-font (font-spec :family "Fira Mono" :size 19))


                                        ;(setq doom-font (font-spec :family "Mononoki Nerd Font" :size 30)
                                        ;      doom-big-font (font-spec :family "Mononoki Nerd Font" :size 36)
                                        ;      doom-variable-pitch-font (font-spec :family "iA Writer Quattro S" :size 24)
                                        ;)



                                        ;(setq doom-font (font-spec :family "iA Writer Quattro S" :size 24)
                                        ;      doom-big-font (font-spec :family "iA Writer Quattro S" :size 36)
                                        ;      doom-variable-pitch-font (font-spec :family "iA Writer Quattro S" :size 24)
                                        ;      doom-serif-font (font-spec :family "iA Writer Quattro S" :weight 'light))

(use-package! rainbow-mode
  :after rainbow-delimiter
:init (rainbow-mode))

(after! nyan-mode
     :init
    (nyan-mode))

(setq display-line-numbers-type 'relative)

(setq evil-move-cursor-back nil)

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
