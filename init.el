;;; init.el --- main config file
;;; Commentary

;;; Code: UI Improvements
(menu-bar-mode -1)              ;; Disable the menu bar
(tool-bar-mode -1)              ;; Disable the toolbar
(scroll-bar-mode -1)            ;; Hide the always-visible scrollbar
(global-display-line-numbers-mode t)
(setq inhibit-splash-screen t)   ;; Remove the "Welcome to GNU Emacs" splash screen
(setq use-file-dialog nil)

(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups"))))
(setq auto-save-file-name-transforms
      `((".*" ,(concat user-emacs-directory "auto-saves/") t)))
(setq create-lockfiles nil)

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

(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta))

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

  :config
  (set-face-attribute 'fill-column-indicator nil
                      :foreground "#717C7C" ; katana-gray
                      :background 'unspecified)
  (global-display-fill-column-indicator-mode 1))

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

;; (global-set-key (kbd "C-x p a") 'projectile-add-known-project)

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
  (load-theme 'doom-pine t))

(use-package mood-line
  :init (mood-line-mode))

(use-package nerd-icons)

;; which-key shows available keybindings after a prefix
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

(use-package swiper
  :bind (("C-s" . swiper)))

(ido-mode t)
(ido-everywhere t)
(use-package smex
  :config
  (smex-initialize))

(use-package projectile
  :init
  (setq projectile-completion-system 'ido)
  (setq projectile-project-search-path '("~/codehub/" "~/Org" "~/.config/"))
  (setq projectile-enable-caching t)
  (setq projectile-mode-line '(:eval (if (string= "-" (projectile-project-name) ) " " (format " [%s] " (projectile-project-name)))))
  :config
  (projectile-mode +1)
  (projectile-discover-projects-in-search-path))

;; Git integration
(use-package magit
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

;; Search project with Ripgrep (rg)
(use-package rg
  ;; Bind rg-menu to C-c s (s for search)
  :bind (("C-c s" . rg-menu)))


;; Garbage Collection optimizer
(use-package gcmh
  :demand
  :config
  (gcmh-mode 1))

;;;----------------------------------------------------------------------------
;;; Programming Language Support
;;;----------------------------------------------------------------------------

;; Auto-completion
(use-package company
  :hook (prog-mode . global-company-mode)
  :config
  (setq company-idle-delay 0.4)
  (setq company-minimum-prefix-length 1))

;; Syntax checking
(use-package flycheck
  :init (global-flycheck-mode))

;; Snippets
(use-package yasnippet
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets)

;; Auto-formatter
(use-package format-all
  :hook (prog-mode . format-all-mode))

;; LSP (Language Server Protocol)
(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l") ;; lsp commands will be under C-c l
  :hook ((rust-mode . lsp)
         (go-mode . lsp)
         (python-mode . lsp)
         (yaml-ts-mode . lsp)
         (yaml-mode . lsp)
         (terraform . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)


;; Make lsp ui great again :)
(use-package lsp-ui
  :commands lsp-ui-mode
  :config
  (global-set-key (kbd "C-c k") 'lsp-ui-doc-glance)
  (setq lsp-ui-doc-delay 0.1)
  (setq lsp-ui-doc-position 'at-point))

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
(use-package terraform-mode)
(use-package yaml-mode)

;;;----------------------------------------------------------------------------
;;; Finalization
;;;----------------------------------------------------------------------------

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("56044c5a9cc45b6ec45c0eb28df100d3f0a576f18eef33ff8ff5d32bac2d9700"
     "599f72b66933ea8ba6fce3ae9e5e0b4e00311c2cbf01a6f46ac789227803dd96"
     default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(use-package hl-todo
  ;; Enable hl-todo automatically in programming modes
  :hook (prog-mode . hl-todo-mode)
  :config
  ;; You can customize the words and their corresponding colors (faces)
  (setq hl-todo-keyword-faces
        '(("TODO"  . "#FFD700") ; A bright yellow
          ("FIXME" . "#FF4500") ; An orangey red
          ("HACK"  . "#FF69B4") ; Hot pink
          ("NOTE"  . "#1E90FF") ; A nice blue
          ("REVIEW". "#9932CC"))))

(setq alfies-preferred-font "Firacode Nerd Font")

(if (find-font (font-spec :name alfies-preferred-font))
    (progn
      (set-face-attribute 'default nil :font alfies-preferred-font :height 160)
      (message "Successfully set font to %s." alfies-preferred-font))
  (message "WARNING: Font '%s' is not installed." alfies-preferred-font))


;;;----------------------------------------------------------------------------
;;; Keybindings
;;;----------------------------------------------------------------------------

;; Bind custom function
;; Better M-x brooooo
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(global-set-key (kbd "C-c C-r") 'reload-init-file)
(global-set-key (kbd "C-c C-c") 'compile)

(global-set-key (kbd "C-x p p") 'projectile-switch-project)
(global-set-key (kbd "C-x p a") 'projectile-add-known-project)
(global-set-key (kbd "C-x p f") 'projectile-find-file)


(global-set-key (kbd "C-x g m") 'magit)
(global-set-key (kbd "C-x w q") 'alfie-close-and-save)
(global-set-key (kbd "C-x C-/") 'comment-line)
(global-set-key (kbd "C-,") 'duplicate-line)
(global-set-key (kbd "C-.") 'copy-from-above-command)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <left>") 'windmove-left)
(global-set-key (kbd "C-c <up>") 'windmove-up)
(global-set-key (kbd "C-c <down>") 'windmove-down)

;; NOTE: This is a prexix :)
(defvar my-find-map (make-sparse-keymap)
  "My custom keymap for various find commands.")
(global-set-key (kbd "C-c f") (cons "Find" my-find-map))
;; init.el ends here
