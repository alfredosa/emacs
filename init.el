;;; init.el --- main config file

;;; Code: UI Improvements
(menu-bar-mode -1)              ;; Disable the menu bar
(tool-bar-mode -1)              ;; Disable the toolbar
(scroll-bar-mode -1)            ;; Hide the always-visible scrollbar
(setq inhibit-splash-screen t)   ;; Remove the "Welcome to GNU Emacs" splash screen
(setq use-file-dialog nil)
(global-display-line-numbers-mode t)

;;;----------------------------------------------------------------------------
;;; Package Management (straight.el)
;;;----------------------------------------------------------------------------
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(setq straight-use-package-by-default t)
(setq use-package-always-defer t)
(setq straight-check-for-modifications '(check-on-save find-when-checking))
(setq straight-vc-git-default-clone-depth 1)

;;;----------------------------------------------------------------------------
;;; Core Emacs Behavior
;;;----------------------------------------------------------------------------
(use-package emacs
  :init
  ;; No welcome message in scratch buffer
  (setq initial-scratch-message nil)

  ;; Use y/n instead of yes/no
  (defalias 'yes-or-no-p 'y-or-n-p)

  ;; Sensible defaults
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 2)
  (setq-default fill-column 88)

  ;; UTF-8 everywhere
  (set-charset-priority 'unicode)
  (setq locale-coding-system 'utf-8
        coding-system-for-read 'utf-8
        coding-system-for-write 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8)
  (prefer-coding-system 'utf-8)
  (setq default-process-coding-system '(utf-8-unix . utf-8-unix))

  ;; macOS-specific UI tweaks
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . light))
  (setq ns-use-proxy-icon  nil)
  (setq frame-title-format nil)

  :config
  ;; Line numbers in programming modes, but not everywhere else.
  (defun ab/enable-line-numbers ()
    "Enable relative line numbers"
    (interactive)
    (setq display-line-numbers t))
  (add-hook 'prog-mode-hook #'ab/enable-line-numbers)

  ;; Disable line numbers in modes where they are not useful
  (dolist (mode '(org-mode-hook
                  term-mode-hook
                  shell-mode-hook
                  eshell-mode-hook
                  magit-status-mode-hook))
    (add-hook mode (lambda () (display-line-numbers-mode 0))))

  ;; Show a fill column indicator
  (set-face-attribute 'fill-column-indicator nil
                      :foreground "#717C7C" ; katana-gray
                      :background nil)
  (global-display-fill-column-indicator-mode 1))

;;;----------------------------------------------------------------------------
;;; Keybindings
;;;----------------------------------------------------------------------------

;; Make Escape quit prompts and other things
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Custom function to reload init file
(defun reload-init-file ()
  "Reload init file."
  (interactive)
  (load-file user-init-file))

(defun alfie-close-and-save ()
  "Master :wq from neovim"
  (interactive)
  (save-buffer)
  (kill-buffer))

;; Bind custom function
(global-set-key (kbd "C-c C-r") 'reload-init-file)
(global-set-key (kbd "C-x p p") 'projectile-switch-project)
(global-set-key (kbd "C-x p a") 'projectile-add-known-project)
(global-set-key (kbd "C-x g m") 'magit)
(global-set-key (kbd "C-x w q") 'alfie-close-and-save)

; (global-set-key (kbd "C-x p a") 'projectile-add-known-project)

;; We will define more keybindings below as we configure packages

;;;----------------------------------------------------------------------------
;;; Essential Packages
;;;----------------------------------------------------------------------------

;; Load shell environment variables
(use-package exec-path-from-shell
  :demand
  :config
  (when (or (memq window-system '(mac ns x))
            (daemonp))
    (exec-path-from-shell-initialize)))

;; UI Theming
(use-package doom-themes
  :demand
  :config
  (load-theme 'doom-gruvbox t))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

(use-package nerd-icons)

;; which-key shows available keybindings after a prefix
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

;; Ivy/Counsel/Swiper completion framework
(use-package ivy
  :demand
  :config
  (ivy-mode 1)
  ;; Make ivy use fuzzy matching
  (setq ivy-re-builders-alist '((t . ivy--regex-fuzzy))))

(use-package counsel
  :after ivy
  :demand
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . counsel-minibuffer-history)))

(use-package swiper
  :after ivy
  :bind (("C-s" . swiper)))

(use-package ivy-prescient
  :after ivy
  :demand
  :config
  (ivy-prescient-mode 1)
  (prescient-persist-mode 1))

;; Project management
(use-package projectile
  :demand
  :init
  (setq projectile-project-search-path '("~/codehub/" "~/Org" "~/.config/"))
  (setq projectile-completion-system 'ivy) ; Make projectile use Ivy
  :config
  (projectile-mode +1))

(use-package counsel-projectile
  :after projectile
  :demand
  :config (counsel-projectile-mode))

;; Git integration
(use-package magit
  :demand
  ;; Bind magit-status to C-c g
  :bind (("C-c g" . magit-status)))

;; THIS IS THE CORRECT CONFIGURATION
(use-package diff-hl
  :config
  ;; The functions diff-hl-magit-pre-refresh and diff-hl-magit-post-refresh
  ;; are only defined AFTER diff-hl is loaded. So, we must add the
  ;; hooks here, in the :config block.
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (global-diff-hl-mode))

;; Integrated Terminal
(use-package vterm)
(use-package vterm-toggle
  ;; Bind vterm-toggle to C-c '
  :bind (("C-c o t" . vterm-toggle)))

;; Search project with Ripgrep (rg)
(use-package rg
  :demand
  ;; Bind rg-menu to C-c s (s for search)
  :bind (("C-c s" . rg-menu)))


;; Garbage Collection optimizer
(use-package gcmh
  :demand
  :config
  (gcmh-mode 1))

;; Emojis!
(use-package emojify
  :hook (after-init . global-emojify-mode))

;;;----------------------------------------------------------------------------
;;; Programming Language Support
;;;----------------------------------------------------------------------------

;; Auto-completion
(use-package company
  :demand
  :hook (prog-mode . global-company-mode)
  :config
  (setq company-idle-delay 0.1)
  (setq company-minimum-prefix-length 1))

;; Syntax checking
(use-package flycheck
  :demand
  :init (global-flycheck-mode))

;; Snippets
(use-package yasnippet
  :demand
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets)

;; Auto-formatter
(use-package format-all
  :demand
  :hook (prog-mode . format-all-mode))

;; LSP (Language Server Protocol)
(use-package lsp-mode
  :demand
  :init
  (setq lsp-keymap-prefix "C-c l") ;; lsp commands will be under C-c l
  :hook ((rust-mode . lsp)
         (go-mode . lsp)
         (python-mode . lsp)
         (yaml-ts-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

(use-package lsp-ui :commands lsp-ui-mode)
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)
(use-package dap-mode) ;; Debugger support

;; Tree-sitter for better syntax parsing
(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;; Language Modes
(use-package org
  :hook (org-mode . visual-line-mode)
  :config
  (setq org-ellipsis " ?")
  (setq org-hide-emphasis-markers t))

(use-package markdown-mode
  :config
  (setq markdown-fontify-code-blocks-natively t))

(use-package zig-mode)
(use-package rust-mode)
(use-package go-mode)
(use-package gotest)
(use-package typescript-mode)


;; This is just so fucking cool. 
(use-package tab-bar
  :ensure nil ; It's built-in, no need to download
  :config
  ;; Enable the tab-bar globally
  (tab-bar-mode 1)

  ;; When switching projects, create a new tab for it
  (setq projectile-switch-project-action #'projectile-dired-in-new-tab)

  ;; Customize the look a bit (optional)
  (setq tab-bar-show 1 ; Show text labels
        tab-bar-close-button-show nil ; Hide the 'x' button
        tab-bar-new-button-show nil   ; Hide the '+' button
        tab-bar-separator nil)        ; Remove separators for a cleaner look

  ;; --- Keybindings for fast switching ---
  ;; Bind M-1, M-2, ... M-9 to switch to the corresponding tab
  (dotimes (i 9)
    (global-set-key (kbd (format "M-%d" (1+ i)))
                    `(lambda ()
                       (interactive)
                       (tab-bar-select-tab ,(1+ i)))))
  ;; Bind M-0 to select the 10th tab (optional)
  (global-set-key (kbd "M-0")
                  '(lambda ()
                     (interactive)
                     (tab-bar-select-tab 10))))

;; Helper function for projectile to open dired in a new tab
(defun projectile-dired-in-new-tab (project)
  "Switch to PROJECT and show its root in Dired, creating a new tab if needed."
  (interactive (list (projectile-project-root)))
  (let ((projectile-switch-project-action 'projectile-dired))
    (persp-projectile-find-project-and-switch project t)))

;;;----------------------------------------------------------------------------
;;; Finalization
;;;----------------------------------------------------------------------------

;; Set GC threshold higher after startup to reduce stuttering
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 16 1024 1024)) ;; 16mb
            (message "Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;;; init.el ends here.
