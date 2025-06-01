;;; init.el --- Gabriel's Emacs configuration

;; Copyright © 2020-2025 Gabriel Marin

;; Author: Gabriel Marin <gbrlmarn@gmail.com>
;; URL: https://github.com/gbrlmarn/.emacs.d

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
      user-mail-address "gbrlmarn@gmail.com")

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

;; show time in mode line
(setq display-time-day-and-date t
      display-time-24hr-format t
      display-time-default-load-average nil
      display-time-interval 60
      display-time-format "%H:%M %d.%m.%Y")
(display-time-mode t)

;; fonts
(cond
 ((find-font (font-spec :name "Monaco"))
  (set-frame-font "Monaco 20"))
 ((find-font (font-spec :name "CodeNewRoman"))
  (set-frame-font "CodeNewRoman 20"))
 ((find-font (font-spec :name "AnonymousPro"))
  (set-frame-font "AnonymousPro 20"))
  ((find-font (font-spec :name "NotoSansMono"))
  (set-frame-font "NotoSansMono 20"))
 ((find-font (font-spec :name "UbuntuMono"))
  (set-frame-font "UbuntuMono 20"))
 )

;; display line numbers
(global-display-line-numbers-mode)
(menu-bar--display-line-numbers-mode-relative)

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

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

(use-package dracula-theme
  :config
  (load-theme 'dracula t)
  (set-face-attribute 'default nil :background "black")
  (set-face-attribute 'line-number nil :background "black"))

(use-package paredit
  :config
  (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
  (add-hook 'lisp-interaction-mode-hook #'paredit-mode)
  (add-hook 'lisp-mode-hook #'paredit-mode)
  (add-hook 'scheme-mode-hook #'paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'paredit-mode))

(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)))

(use-package vterm)

(use-package pdf-tools
  :config
  (pdf-tools-install)
  (setq pdf-view-midnight-colors '("#ffffff" . "#000000"))
  (add-hook 'pdf-view-mode-hook (lambda () (display-line-numbers-mode 0))))

(use-package clojure-mode
  :config
  (define-clojure-indent
   (returning 1)
   (testing-dynamic 1)
   (testing-print 1))
  (add-hook 'clojure-mode-hook #'paredit-mode)
  (add-hook 'clojure-mode-hook #'subword-mode)
  (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode))

(use-package inf-clojure
  :config
  (add-hook 'inf-clojure-mode-hook #'paredit-mode)
  (add-hook 'inf-clojure-mode-hook #'rainbow-delimiters-mode))

(use-package cider
  :config
  (setq nrepl-log-messages t)
  (add-hook 'cider-repl-mode-hook #'paredit-mode)
  (add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode))

(use-package slime
  :config
  (setq inferior-lisp-program "sbcl")
  (add-hook 'lisp-mode-hook #'paredit-mode)
  (add-hook 'lisp-mode-hook #'rainbow-delimiters-mode))

(use-package erlang)
(use-package elixir-mode)
(use-package go-mode)
(use-package typescript-mode)
(use-package markdown-mode)
(use-package yaml-mode)
(use-package protobuf-mode)

(use-package company
  :config
  (global-company-mode))

(use-package lsp-mode
  :init (setq lsp-keymap-prefix "C-c l")
  :commands lsp)

(use-package elfeed)
(use-package elfeed-org
  :init
  (setq rmh-elfeed-org-files
        (list "~/.emacs.d/elfeed.org"))
  :config
  (elfeed-org))

(use-package emms
  :config
  (emms-all)
  (emms-default-players)
  :bind (("C-c C-r" . emms-toggle-repeat-track)))

(use-package org-drill
  :config
  (setq org-drill-learn-fraction 0.8))
(use-package org-drill-table)

(use-package wgrep)

(use-package bash-completion
  :config
  (bash-completion-setup))

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
