(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                   ("melpa" . "https://melpa.org/packages/")))
;;(add-to-list 'package-archives
;;	     '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(defvar my-packages '(;; Put packages here...
		      magit
		      doom-themes
		      bongo
		      volume
		      ))
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

(setq initial-frame-alist
'((top . 0) (left . -1) (width 60) (height . 80)))
(setq display-time-day-and-date t)

(set-face-attribute 'default nil :font "Liberation Mono-16")
  (use-package ir-black-theme
  :ensure t
  :config
  (load-theme 'ir-black t)

;; Corects (and improves) org-mode' native fontificaion.
(doom-themes-org-config))

(display-time-mode)
