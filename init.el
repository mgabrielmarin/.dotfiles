;;; init.el --- Gabriel's Emacs configuration
;;
;; Copyright © 2020-2023 Gabriel Marin
;;
;; Author: Gabriel Marin <gbrlmarn@proton.me>
;; URL: https://github.com/gbrlmarn/.emacs.d

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
               

;; Remove blinking cursor
(blink-cursor-mode -1)

;; Disable bell ring
(setq ring-bell-function 'ignore)

;; Disable startup screen
(setq inhibit-startup-screen t)

;; Nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

(when (fboundp 'pixel-scroll-precision-mode)
  (pixel-scroll-precision-mode t))

;; Fonts
(cond
 ((find-font (font-spec :name "Cascadia Code"))
  (set-frame-font "Cascadia Code-18"))
 ((find-font (font-spec :name "Menlo"))
  (set-frame-font "Menlo-18"))
 ((find-font (font-spec :name "DejaVu Sans Mono"))
  (set-frame-font "DejaVu Sans Mono-18"))
 ((find-font (font-spec :name "Inconsolata"))
  (set-frame-font "Inconsolata-18")))

;; mode line settigs
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; Indentation
(setq-default indent-tabs-mode nil) ;; don't use tab to indent
(setq-default tab-width 8)

;; Newline at the end of file
(setq require-final-newline t)

;; Wrap lines at 80 chars
(setq-default fill-column 80)

;; delete the selection with a keypress
(delete-selection-mode t)

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
  (set-face-foreground 'font-lock-comment-face "darkgreen")
  (set-face-foreground 'font-lock-comment-delimiter-face "darkgreen"))

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

(use-package cider
  :ensure t
  :config
  (setq nrepl-log-messages t)
  (add-hook 'cider-repl-mode-hook #'paredit-mode)
  (add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode))

(use-package flycheck-joker
  :ensure t)

(use-package elixir-mode
  :ensure t
  :config
  (add-hook 'elixir-mode #'subword-mode))

(use-package erlang
  :ensure t)

(use-package markdown-mode
  :ensure t)

(use-package flycheck
  :ensure t
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package company
  :ensure t)

(use-package ivy
  :ensure t)

(use-package elfeed
  :ensure t)
(use-package  elfeed-org
  :ensure t
  :config
  (setq rmh-elfeed-org-files
        (list "~/.emacs.d/elfeed.org")))

;; config changes made through the customize UI will be stored here
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(when (file-exists-p custom-file)
  (load custom-file))

;;; init.el ends here
