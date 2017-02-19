
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(require 'org-install)
(require 'ob-tangle)
(org-babel-load-file (expand-file-name "newConfig.org" user-emacs-directory))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (zenburn-theme zenburn yaml-mode yafolding writegood-mode window-numbering which-key web-mode use-package solarized-theme smex smartparens rainbow-delimiters projectile powerline org-bullets neotree multiple-cursors monokai-theme marmalade markdown-mode magit ido-grid-mode htmlize helm haml-mode guide-key graphviz-dot-mode flycheck find-file-in-repository feature-mode expand-region evil ess ensime engine-mode dumb-jump doom-themes autopair ace-jump-mode ac-slime))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
