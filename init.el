;; Do no show the startup screen
(setq inhibit-startup-message t)

;; Require and initialize package
(require 'package)

(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/") t)

(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Package list
(defvar my-packages '(magit
		      cider
		      clojure-mode
		      ))

;; Automataly install any missing packages
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; My theme 
(use-package zenburn-theme
  :ensure t
  :config
  (load-theme 'zenburn t))

(use-package magit
  :ensure t)

;; Disable toolbar
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Frame size
(add-to-list 'default-frame-alist '(height . 55))
(add-to-list 'default-frame-alist '(width . 90))
(add-to-list 'default-frame-alist '(top . 0))
(add-to-list 'default-frame-alist '(left . -1))

;; Text size and font
(set-face-attribute 'default nil :font "Monospace-15")

;; Add line numbers
(global-display-line-numbers-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(clojure-mode magit zenburn-theme use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
