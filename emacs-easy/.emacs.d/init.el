;;; init.el --- Emacs configuration with CUA and Modus Vivendi -*- lexical-binding: t -*-

;;; Commentary:
;; A clean Emacs setup with Common User Access keybindings,
;; dark Modus Vivendi theme, and emacsclient support.

;;; Code:

;; =============================================================================
;; Startup Performance
;; =============================================================================

;; Reduce garbage collection during startup
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 16 1024 1024)))) ; 16MB after startup

;; =============================================================================
;; UI Configuration
;; =============================================================================

;; Hide scroll bars (works in both GUI and terminal)
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; Hide menu bar and tool bar
(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;; Disable the startup screen
(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

;; Disable the mode line
(setq-default mode-line-format nil)

;; Show matching parentheses
(show-paren-mode 1)

;; Pixel-wise frame resizing (no gaps at edges)
(setq frame-resize-pixelwise t)

;; Smoother scrolling
(pixel-scroll-precision-mode 1)

;; =============================================================================
;; Modus Vivendi Theme (Dark)
;; =============================================================================

;; Modus themes are built into Emacs 28+
;; For older versions, you may need to install them
(require 'modus-themes nil t)

;; Theme customization (before loading)
(setq modus-themes-italic-constructs t)
(setq modus-themes-bold-constructs t)
(setq modus-themes-region '(bg-only))
(setq modus-themes-org-blocks 'tinted-background)

;; Load the dark theme
(load-theme 'modus-vivendi t)

;; Override background and fringe colors
(set-face-background 'default "#000000")
(set-face-background 'fringe "#000000")

;; Don't highlight trailing whitespace
(setq-default show-trailing-whitespace nil)

;; =============================================================================
;; CUA Mode (Common User Access)
;; =============================================================================

;; Enable CUA mode for familiar keybindings:
;; C-c = Copy, C-x = Cut, C-v = Paste, C-z = Undo
(cua-mode 1)

;; Note: CUA mode intelligently handles C-x and C-c:
;; - When region is active: C-x cuts, C-c copies
;; - When no region: C-x and C-c work as normal Emacs prefix keys

;; =============================================================================
;; Emacs Server (for emacsclient)
;; =============================================================================

;; Start server if not already running
;; This allows using `emacsclient` to open files in an existing Emacs instance
(require 'server)
(unless (server-running-p)
  (server-start))

;; When using emacsclient, don't open a new frame if one exists
(setq server-window 'pop-to-buffer)

;; =============================================================================
;; Frame Settings (for new frames via emacsclient -c)
;; =============================================================================

;; Ensure new frames also get proper UI settings
(defun my/frame-setup (&optional frame)
  "Configure FRAME (or current frame) with proper UI settings."
  (with-selected-frame (or frame (selected-frame))
    (when (display-graphic-p)
      (scroll-bar-mode -1)
      ;; (set-face-background 'default "#212121")
      ;; (set-face-background 'fringe "#212121")

      )))

;; Apply to current and future frames
(my/frame-setup)
(add-hook 'after-make-frame-functions #'my/frame-setup)

;; =============================================================================
;; Package Management
;; =============================================================================

;; Initialize package sources
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)

;; use-package is built into Emacs 29+
;; For older versions, install it
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;; =============================================================================
;; Magit
;; =============================================================================

(use-package magit
  :bind ("C-x g" . magit-status)
  :config
  (defun my/magit-quick-push (message)
    "Stage all, commit with MESSAGE, push to origin, and close magit."
    (interactive "sCommit message: ")
    (magit-commit-create (list "-m" message))
    (magit-run-git-async "push" "origin" (magit-get-current-branch))
    (magit-process-wait)
    (when (zerop magit-this-error)
      (magit-mode-bury-buffer)))
  (define-key magit-mode-map (kbd "C-c p") #'my/magit-quick-push))

;; =============================================================================
;; Org Mode
;; =============================================================================

(use-package org
  :ensure nil  ; org is built-in
  :config
  (set-face-attribute 'org-ellipsis nil :underline nil)
  (setq org-ellipsis " ▼"
        org-hide-emphasis-markers t
        org-clock-mode-line-total 'today
        org-duration-format (quote h:mm))
  (add-hook 'org-mode-hook (lambda ()
                             (org-indent-mode)
                             (variable-pitch-mode -1)
                             (auto-fill-mode 1))))

;; =============================================================================
;; Compilation
;; =============================================================================

;; Auto-close compilation buffer on success
(defun my/compilation-finish (buf status)
  "Close compilation BUF if STATUS indicates no errors."
  (when (string-match-p "finished" status)
    (run-at-time 1 nil #'delete-windows-on buf)))
(add-hook 'compilation-finish-functions #'my/compilation-finish)

;; =============================================================================
;; Better Defaults
;; =============================================================================

;; Use UTF-8 everywhere
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; Typed text replaces selection
(delete-selection-mode 1)

;; Auto-revert files when changed on disk
(global-auto-revert-mode 1)

;; Remember cursor position in files
(save-place-mode 1)

;; Save minibuffer history
(savehist-mode 1)

;; Recent files
(recentf-mode 1)
(setq recentf-max-saved-items 100)

;; Don't litter the file system with backup files
(setq backup-directory-alist `(("." . ,(expand-file-name "backups" user-emacs-directory))))
(setq auto-save-file-name-transforms `((".*" ,(expand-file-name "auto-saves/" user-emacs-directory) t)))
(make-directory (expand-file-name "auto-saves/" user-emacs-directory) t)

;; Single space after period ends sentence
(setq sentence-end-double-space nil)

;; y/n instead of yes/no
(setq use-short-answers t)

;; Don't show trailing whitespace (avoid theme-specific colored backgrounds)
(setq-default show-trailing-whitespace nil)

;; Use spaces instead of tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; Enable disabled commands
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; =============================================================================
;; Custom File
;; =============================================================================

;; Keep customize settings in a separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;;; init.el ends here

