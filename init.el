;; Remove start page
(setq inhibit-startup-screen t)

;; The source
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")))
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; My packages
(defvar my-packages 
  '(better-defaults
    projectile
    clojure-mode
    cider
    magit
    org
    elfeed
    elfeed-org))

(dolist (p my-packages)
  (unless (package-installed-p p)
    (package-install p)))

;; Elfeed
(require 'elfeed-org)
(elfeed-org)
(setq rmh-elfeed-org-files (list "~/.emacs.d/elfeed.org"))

;; Org mode config
(require 'org)

;; Emacs theme
(use-package zenburn-theme
             :ensure t
             :config
             (load-theme 'zenburn t))

;; Add line numbers global
(global-display-line-numbers-mode)

;; Text font
(set-face-attribute 'default nil :font  "Monospace-15")

;; Emacs position and size
(setq initial-frame-alist '((top . 0) (left . -1) (width . 110) (height . 65)))

;; Add lein path
(setq exec-path (append '("/usr/local/bin" "/usr/local/sbin" "/opt/homebrew/bin" "/opt/homebrew/sbin")
                        exec-path))
                          

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files '("~/fun/emacs/first.org"))
 '(package-selected-packages
   '(elfeed elfeed-org cider clojure-mode projectile better-defaults)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'upcase-region 'disabled nil)

(setq line-number-mode t)
(setq global-display-line-numbers-mode 1)

(set-face-foreground 'font-lock-comment-face "green")

