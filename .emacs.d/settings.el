(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(defvar my-packages '(;; Put packages here...
		      ir-black-theme
		      auto-complete
		      geiser
		      magit
		      bongo
		      volume
		      ))
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(require 'auto-complete)
(global-auto-complete-mode t)

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-screen t)

(display-time-mode)
  (set-face-attribute 'default nil :font "Liberation Mono-16")
  (use-package ir-black-theme
    :ensure t
    :config
    (load-theme 'ir-black t))
  ;; Set transparency
(set-frame-parameter (selected-frame) 'alpha '(70 70))
(add-to-list 'default-frame-alist '(alpha 70 70))
