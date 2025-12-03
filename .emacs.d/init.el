;;; init.el --- Gabriel Marin Emacs configuration

;; Copyright © 2020-2025 Gabriel Marin

;; Author: Gabriel Marin <gabrielmarindev@gmail.com>
;; URL: https://github.com/mgabrielmarin/.emacs.d

;;; Commentary:

;; My personal Emacs configuration.

;;; Code:

(require 'package)

(add-to-list 'package-archives
	         '("melpa" . "https://melpa.org/packages/") t)

;; keep the installed packages in .emacs.d
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))
(when (< emacs-major-version 27)
  (package-initialize))

;; update the package metadata
(unless package-archive-contents
  (package-refresh-contents))

;; Username and Email
(setq user-full-name "Gabriel Marin"
      user-mail-address "gabrielmarindev@gmail.com")

;; Always load newest byte code
(setq load-prefer-newer t)

;; quit Emacs directly even if there are running processes
(setq confirm-kill-processes nil)

;; default modes
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(blink-cursor-mode 0)
(show-paren-mode t)
(pixel-scroll-precision-mode t)

;; default buffer completion
(if (> emacs-major-version 27)
    (fido-vertical-mode)
  (fido-mode))

;; default vars
(setq inhibit-startup-screen t)
(setq ring-bell-function 'ignore)

;; mode line settings
(line-number-mode t)
(column-number-mode t)
;;(size-indication-mode t)

;; theme
(if (>= emacs-major-version 28)
    (load-theme 'modus-vivendi t))

;; fonts
(cond
 ((find-font (font-spec :name "Monaco"))
  (set-frame-font "Monaco 20"))
 ((find-font (font-spec :name "CodeNewRoman"))
  (set-frame-font "CodeNewRoman 20"))
 ((find-font (font-spec :name "Google Sans Code"))
  (set-frame-font "Google Sans Code 20"))
 ((find-font (font-spec :name "AnonymousPro"))
  (set-frame-font "AnonymousPro 20"))
 ((find-font (font-spec :name "NotoSansMono"))
  (set-frame-font "NotoSansMono 20"))
 ((find-font (font-spec :name "UbuntuMono"))
  (set-frame-font "UbuntuMono 20"))
 )

;; display line numbers
;; (global-display-line-numbers-mode -1)
;; (menu-bar--display-line-numbers-mode-relative -1)

;; Indentation
(setq-default indent-tabs-mode nil) ;; don't use tab to indent
(setq-default tab-width 4)

;; Newline at the end of file
(setq require-final-newline t)

;; Wrap lines at 80 chars
(setq-default fill-column 80)

;; delete the selection with a keypress
(delete-selection-mode t)

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; revert buffers automatically when underlying files are changed externaly
(global-auto-revert-mode t)

;; use utf-8
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; smart tab behavior -indent or complete
(setq tab-always-indent 'complete)

;; Start in full screen
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; Use eww by default
(setq browse-url-browser-function 'eww-browse-url)

;; check OS type
(cond
 ((string-equal system-type "windows-nt") ; Microsoft Windows
  (progn (message "Microsoft Windows")))
 ((string-equal system-type "darwin") ; Mac OS X
  (progn (setq mac-right-command-modifier 'meta)
         (setq mac-command-modifier 'meta)
         (message "MacOS")))
 ((string-equal system-type "gnu/linux") ; linux
  (progn (message "Linux"))))

;; Install use package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)
(setq use-package-verbose t)

;; Packages
(use-package magit)
(use-package clojure-mode)
(use-package cider) ;; repl integration for clojure
(use-package erlang)
(use-package elixir-mode)
(use-package go-mode)
(use-package typescript-mode)
(use-package markdown-mode)
(use-package yaml-mode)
(use-package protobuf-mode)
(use-package wgrep)

;; emualte terminals, both are ok
;;(use-package vterm)
;;(use-package eat)

(use-package slime
  :config
  (setq inferior-lisp-program "sbcl"))

(use-package company
  :config
  (global-company-mode))

(use-package bash-completion
  :config
  (bash-completion-setup))

(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)))

(use-package pinentry
  ;; :defer nil ;; NOTE: not sure if this is needed
  :config
  (pinentry-start))

(use-package geiser
  :config
  (use-package geiser-guile))

;; config changes made through the customize UI will be stored here
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(when (file-exists-p custom-file)
  (load custom-file))

;;; init.el ends here
