;;; init.el --- Gabriel's Emacs configuration
;;
;; Copyright © 2020-2024 Gabriel Marin
;;
;; Author: Gabriel Marin <marin.gabriel@protonmail.com>
;; URL: https://github.com/gbrlmarn/.emacs.d
;;
;;; Commentary:
;;
;; My personal Emacs configuration.
;;
;;; Code:

(require 'package)

(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)
;; keep the installed packages in .emacs.d
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))
(package-initialize)
;; update the package metadata
(unless package-archive-contents
  (package-refresh-contents))

;; Always load newest byte code
(setq load-prefer-newer t)

;; quit Emacs directly even if there are running processes
(setq confirm-kill-processes nil)

;; Remove top toolbar
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;; Remove menu bar
(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))

;; Remove scroll bar
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
               
;; Remove blinking cursor
(blink-cursor-mode -1)

;; Disable bell ring
(setq ring-bell-function 'ignore)

;; Disable startup screen
(setq inhibit-startup-screen t)

(when (fboundp 'pixel-scroll-precision-mode)
  (pixel-scroll-precision-mode t))

;; Fonts
(cond
 ((find-font (font-spec :name "DejaVu Sans Mono"))
  (set-frame-font "DejaVu Sans Mono-18"))
 ((find-font (font-spec :name "Cascadia Code"))
  (set-frame-font "Cascadia Code-18"))
 ((find-font (font-spec :name "Menlo"))
  (set-frame-font "Menlo-18"))
 ((find-font (font-spec :name "Inconsolata"))
  (set-frame-font "Inconsolata-18")))
(set-fontset-font "fontset-default" 'symbol "Noto Color Emoji") ;; emoji support

;; mode line settigs
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)
(global-display-line-numbers-mode t)

;; Set time in status bar
(setq display-time-day-and-date t
      display-time-24hr-format t
      display-time-default-load-average nil
      display-time-interval 60
      display-time-format " %a%e %b, %H:%M ")
(display-time-mode t)

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
  (progn
    (message "Microsoft Windows")))
 ((string-equal system-type "darwin") ; Mac OS X
  (progn
    ;; (setq mac-right-command-modifier      'super
    ;;       ns-right-command-modifier       'super
    ;;       mac-option-modifier             'meta
    ;;       ns-option-modifier              'meta
    ;;       mac-right-option-modifier       'none
    ;;       ns-right-option-modifier        'none
    ;;       )
    (setq mac-right-option-modifier 'meta
          ns-right-option-modifier 'meta)
    (message "MacOS")))
 ((string-equal system-type "gnu/linux") ; linux
  (progn
    (message "Linux"))))

;; Install use package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-verbose t)

;; Packages
(use-package paren
  :config
  (show-paren-mode +1))

(use-package magit
  :ensure t)

(use-package dracula-theme
  :ensure t
  :config
  (load-theme 'dracula t)
  (set-background-color "black")
  (set-face-attribute 'line-number nil :background "black"))

(use-package diminish
  :ensure t
  :config
  (diminish 'abbrev-mode)
  (diminish 'flyspell-mode)
  (diminish 'flyspell-prog-mode)
  (diminish 'eldoc-mode))

(use-package paredit
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
  (add-hook 'lisp-interaction-mode-hook #'paredit-mode)
  (add-hook 'lisp-mode-hook #'paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'paredit-mode)
  (diminish 'paredit-mode "()"))

(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)))

(use-package pdf-tools
  :ensure t
  :config
  (pdf-tools-install)
  (setq pdf-view-midnight-colors '("#ffffff" . "#000000"))
  (add-hook 'pdf-view-mode-hook (lambda () (display-line-numbers-mode -1))))

(use-package rainbow-delimiters
  :ensure t)

(use-package rainbow-mode
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-mode)
  (diminish 'rainbow-mode))

(use-package clojure-mode
  :ensure t
  :config
  (define-clojure-indent
    (returning 1)
    (testing-dynamic 1)
    (testing-print 1))
  
  (add-hook 'clojure-mode-hook #'paredit-mode)
  (add-hook 'clojure-mode-hook #'subword-mode)
  (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode))

(use-package inf-clojure
  :ensure t
  :config
  (add-hook 'inf-clojure-mode-hook #'paredit-mode)
  (add-hook 'inf-clojure-mode-hook #'rainbow-delimiters-mode))

(use-package lisp-mode
  :config
  (add-hook 'lisp-mode-hook #'paredit-mode)
  (add-hook 'lisp-mode-hook #'rainbow-delimiters-mode))

(use-package cider
  :ensure t
  :config
  (setq nrepl-log-messages t)
  (add-hook 'cider-repl-mode-hook #'paredit-mode)
  (add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode))

(use-package slime
  :ensure t
  :config
  (setq inferior-lisp-program "sbcl")
  (add-hook 'lisp-mode-hook #'paredit-mode)
  (add-hook 'lisp-mode-hook #'rainbow-delimiters-mode))

(use-package elixir-mode
  :ensure t
  :config
  (add-hook 'elixir-mode #'subword-mode))

(use-package erlang
  :ensure t)

(use-package go-mode
  :ensure t)

(use-package markdown-mode
  :ensure t)

(use-package lsp-mode
  :ensure t
  :init (setq lsp-keymap-prefix "C-c l")
  :commands lsp)

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode))

(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0.5)
  (setq company-show-quick-access t)
  (setq company-tooltip-limit 10)
  (setq company-minimum-prefix-length 2)
  (setq company-tooltip-align-annotations t)
  ;; invert the navigation direction if the the completion popup-isearch-match
  ;; is displayed on top (happens near the bottom of windows)
  (setq company-tooltip-flip-when-above t)
  (global-company-mode)
  (diminish 'company-mode))


(use-package tree-sitter
  :ensure t
  :config
  (add-hook 'after-init-hook #'global-tree-sitter-mode))

(use-package ivy
  :ensure t)

(use-package elfeed
  :ensure t
  :config
  (elfeed-org))
(use-package  elfeed-org
  :ensure t
  :config
  (setq rmh-elfeed-org-files
        (list "~/.emacs.d/elfeed.org")))

(use-package crux
  :ensure t
  :bind (("C-c o" . crux-open-with)
         ("C-c e" . crux-eval-and-replace)
         ("C-c w" . crux-swap-windows)
         ("C-c S" . crux-find-shell-init-file)
         ("C-c s" . crux-ispell-word-then-abbrev)))

(use-package emms
  :ensure t
  :config
  (emms-all)
  (emms-default-players)
  :bind (("C-c C-r" . emms-toggle-repeat-track)))

(use-package org-drill
  :ensure t
  :config
  (setq org-drill-learn-fraction 0.8))
(use-package org-drill-table
  :ensure t)

(use-package undo-tree
  :ensure t
  :config
  ;; autosave the undo-tree history
  (setq undo-tree-history-directory-alist
        `((".*" . ,temporary-file-directory)))
  (setq undo-tree-auto-save-history t)
  (global-undo-tree-mode +1)
  (diminish 'undo-tree-mode))

(use-package which-key
  :ensure t
  :config (which-key-mode))

(use-package wgrep
  :ensure t)

;; config changes made through the customize UI will be stored here
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(when (file-exists-p custom-file)
  (load custom-file))

;;; init.el ends here
