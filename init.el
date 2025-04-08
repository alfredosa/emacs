;;; init.el --- main config file 

;;; Code: UI Improvements
(menu-bar-mode -1)          ;; Disable the menu bar
(tool-bar-mode -1)          ;; Disable the toolbar
(scroll-bar-mode -1)           ; Hide the always-visible scrollbar
(setq inhibit-splash-screen t) ; Remove the "Welcome to GNU Emacs" splash screen
(setq use-file-dialog nil) (global-display-line-numbers-mode t)


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

;; --------- EMACS NICETIES --------
;; define emacs-specific settigs with use-package emacs
(use-package emacs
  :init
  (setq initial-scratch-message nil)
  (defun display-startup-echo-area-message ()
    (message "")))

(use-package emacs
  :init
  (defalias 'yes-or-no-p 'y-or-n-p))

;; Somehow recommended to ensure utf-8?
(use-package emacs
  :init
  (set-charset-priority 'unicode)
  (setq locale-coding-system 'utf-8
        coding-system-for-read 'utf-8
        coding-system-for-write 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8)
  (prefer-coding-system 'utf-8)
  (setq default-process-coding-system '(utf-8-unix . utf-8-unix)))

(use-package emacs
  :init
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 2))

;; Custom keybindings
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Define your custom functions here
(defun reload-init-file ()
  "Reload init file."
  (interactive)
  (load-file user-init-file))

(global-set-key (kbd "C-c r") 'reload-init-file)

(use-package emacs
  :init
  (defun ab/enable-line-numbers ()
    "Enable relative line numbers"
    (interactive)
    (display-line-numbers-mode)
    (setq display-line-numbers 'relative))
  (add-hook 'prog-mode-hook #'ab/enable-line-numbers))

(use-package emacs
  :init
  (setq-default fill-column 88)
  (set-face-attribute 'fill-column-indicator nil
                      :foreground "#717C7C" ; katana-gray
                      :background "transparent")
  (global-display-fill-column-indicator-mode 1))

(use-package emacs
  :init
	(global-set-key (kbd "<escape>") 'keyboard-escape-quit))

(use-package emacs
  :init
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . light))
  (setq ns-use-proxy-icon  nil)
  (setq frame-title-format nil))

(use-package emacs
  :init
  ;; Set the default font
  (set-face-attribute 'default nil
                      :family "JetBrainsMono Nerd Font"
                      :height 120
                      :weight 'normal
                      :width 'normal)

  ;; Optional: Set a different font for variable-pitch-mode
  (set-face-attribute 'variable-pitch nil
                      :family "JetBrainsMono Nerd Font"
                      :height 120
                      :weight 'normal
                      :width 'normal))

;; -------- END EMACS Specific niceties ---------
;; TODO: I think maybe this might be the perfect setup.

;; QUESTION: Does this actually execute???
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))



;; ############ UI Beautify ##############

(use-package doom-themes
  :demand
  :config
  (load-theme 'doom-gruvbox t))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

(use-package nerd-icons)

;; wtf
(use-package nyan-mode
  :init
  (nyan-mode))

;; Which-key helps you remember key bindings, feel right at home?
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3)) ;; I like the 0.3 :D 
;;


(use-package general
  :demand
  :config
  (general-evil-setup)

  (general-create-definer leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "C-SPC")

  (leader-keys
    "x" '(execute-extended-command :which-key "execute command")
    "r" '(restart-emacs :which-key "restart emacs")
    "i" '((lambda () (interactive) (find-file user-init-file)) :which-key "open init file")

    ;; File operations - similar to Doom's SPC f bindings
    "f" '(:ignore t :which-key "find/file")
    "f <escape>" '(keyboard-escape-quit :which-key t)
    "f f" '(counsel-find-file :which-key "find file")
    "f r" '(counsel-recentf :which-key "recent files")
    "f z" '(counsel-fzf :which-key "fzf")
    "f g" '(counsel-git :which-key "find git file")
    
    ;; Search operations - similar to Doom's SPC s bindings
    "s" '(:ignore t :which-key "search")
    "s <escape>" '(keyboard-escape-quit :which-key t)
    "s s" '(swiper :which-key "search buffer")
    "s p" '(counsel-rg :which-key "search project")
    "s d" '(counsel-rg :which-key "search directory")
    "s i" '(counsel-imenu :which-key "search imenu")
    "s b" '(counsel-switch-buffer :which-key "search buffers")
    
    ;; Project operations - additional useful bindings
    "p f" '(counsel-projectile-find-file :which-key "find file in project")
    "p s" '(counsel-projectile-rg :which-key "search in project")
    
    ;; Buffer operations - additional buffer commands
    "b i" '(ibuffer :which-key "ibuffer")
    "b k" '(kill-current-buffer :which-key "kill buffer")
    "b n" '(next-buffer :which-key "next buffer")
    "b p" '(previous-buffer :which-key "previous buffer")
    
    ;; Direct access to rg (ripgrep)
    "/" '(counsel-rg :which-key "search project")  )
)

;; ;; I have no clue what this is?
;; ;; Better completion framework
(use-package ivy
  :demand
  :config
  (ivy-mode))

;; Optional: Fuzzy matching for Ivy
(use-package flx  ; provides fuzzy matching for ivy
  :demand
  :config
  (setq ivy-re-builders-alist
        '((t . ivy--regex-fuzzy))))

;; Optional: Better sorting for ivy results
(use-package ivy-prescient
  :after ivy
  :demand
  :config
  (ivy-prescient-mode 1)
  (prescient-persist-mode 1))

;;
(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . counsel-minibuffer-history)))
;;
(use-package swiper
  :bind (("C-s" . swiper)))

;; Project management
(use-package projectile
  :demand
  :general
  (leader-keys
    :states 'normal
    "SPC" '(projectile-find-file :which-key "find file")

    ;; Buffers
    "b b" '(projectile-switch-to-buffer :which-key "switch buffer")

    ;; Projects
    "p" '(:ignore t :which-key "projects")
    "p <escape>" '(keyboard-escape-quit :which-key t)
    "p p" '(projectile-switch-project :which-key "switch project")
    "p a" '(projectile-add-known-project :which-key "add project")
    "p r" '(projectile-remove-known-project :which-key "remove project"))
  :init
  (setq projectile-project-search-path '("~/codehub/"))
  (setq projectile-switch-project-action #'projectile-dired)
  (projectile-mode +1))
;;
(use-package counsel-projectile
  :config (counsel-projectile-mode))
;;
;; ;; Git integration
;; (use-package magit
(use-package magit
  :demand
  :general
  (leader-keys
    "g" '(:ignore t :which-key "git")
    "g <escape>" '(keyboard-escape-quit :which-key t)
    "g g" '(magit-status :which-key "status")
    "g l" '(magit-log :which-key "log"))
  (general-nmap
    "<escape>" #'transient-quit-one))

(setq evil-want-keybinding nil)

(use-package evil
  :demand ; No lazy loading
  :config
  (evil-mode 1))


(use-package evil-nerd-commenter
  :general
  (general-nvmap
    "gc" 'evilnc-comment-operator))

(use-package evil-collection
  :after evil
  :demand
  :config
  (evil-collection-init))


(use-package diff-hl
  :init
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  :config
  (global-diff-hl-mode))

(use-package vterm)

(use-package vterm-toggle
  :general
  (leader-keys
    "'" '(vterm-toggle :which-key "terminal")))


(use-package gcmh
  :demand
  :config
  (gcmh-mode 1))


;; For programming
(use-package company
  :demand
  :config
  (setq company-idle-delay 0.1)
  (setq company-minimum-prefix-length 1)
  (global-company-mode t))

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

(use-package format-all
  :demand
  :ensure t
  :hook (prog-mode . format-all-mode))

;; Org mode enhancements
;; hook seems to be the equivalent of on-attach buffer from Vim;
(use-package org
  :hook (org-mode . visual-line-mode)
  :config
  (setq org-ellipsis " ▾")
  (setq org-hide-emphasis-markers t))

;;
;; Keep customizations in a separate file
;; (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
;; (when (file-exists-p custom-file)
;;   (load custom-file))

;; @@@@@@@@@@@@@@ LSP @@@@@@@@@@
(use-package lsp-mode
  :demand
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (rust-mode . lsp)
         (go-mode . lsp)
         (python-mode . lsp)
         (yaml-ts-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

;; optional
(use-package lsp-ui :commands lsp-ui-mode)
;; if you are ivy user
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

;; optionally if you want to use debugger
(use-package dap-mode)
;; TODO: add dap language.
;; (use-package dap-LANGUAGE) to load the dap adapter for your language
;; @@@@@@@@@@@@@@ LSP @@@@@@@@@@


(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package markdown-mode
  :config
  (setq markdown-fontify-code-blocks-natively t))
(use-package zig-mode
  :general
  (leader-keys
    "m" '(:ignore t :which-key "mode")
    "m <escape>" '(keyboard-escape-quit :which-key t)
    "m b" '(zig-compile :which-key "build")
    "m r" '(zig-run :which-key "run")
    "m t" '(zig-test :which-key "test")
    ))
(use-package rust-mode
  :general
  (leader-keys
    "m" '(:ignore t :which-key "mode")
    "m <escape>" '(keyboard-escape-quit :which-key t)
    "m b" '(rust-compile :which-key "build")
    "m r" '(rust-run :which-key "run")
    "m t" '(rust-test :which-key "test")
    "m k" '(rust-check :which-key "check")
    "m c" '(rust-run-clippy :which-key "clippy")
    "m f" '(rust-format-buffer :which-key "format")
    ))

(use-package go-mode)
(use-package gotest
  :general
  (leader-keys
    "m" '(:ignore t :which-key "mode")
    "m <escape>" '(keyboard-escape-quit :which-key t)
    "m t" '(go-test-current-project :which-key "test")
    "m r" '(go-run :which-key "run")))
(use-package typescript-mode)

(use-package rg
  :demand
  :general
  (leader-keys
    "f" '(rg-menu :which-key "find")))


;; Reset GC threshold after startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold 16777216) ;; 16mb
            (message "Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

