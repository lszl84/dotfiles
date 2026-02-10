;;; init.el --- Emacs configuration for ASP.NET Core Razor Pages development -*- lexical-binding: t -*-

;;; Commentary:
;; A clean Emacs setup with CUA keybindings, Modus Vivendi theme,
;; and full support for running and debugging ASP.NET Core Razor Pages
;; applications via LSP (OmniSharp), DAP (netcoredbg), and web-mode.

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

;; Smoother scrolling
(setq scroll-margin 3)
(setq scroll-conservatively 101)
(setq scroll-preserve-screen-position t)

;; Line numbers for code buffers
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'web-mode-hook #'display-line-numbers-mode)

;; =============================================================================
;; Modus Vivendi Theme (Dark)
;; =============================================================================

;; Modus themes are built into Emacs 28+
(require 'modus-themes nil t)

;; Theme customization (before loading)
(setq modus-themes-italic-constructs t)
(setq modus-themes-bold-constructs t)
(setq modus-themes-region '(bg-only))
(setq modus-themes-org-blocks 'tinted-background)

;; Load the dark theme
(load-theme 'modus-vivendi t)

;; Override background and fringe colors
(set-face-background 'default "#222226")
(set-face-background 'fringe "#222226")

;; Don't highlight trailing whitespace
(setq-default show-trailing-whitespace nil)

;; =============================================================================
;; CUA Mode (Common User Access)
;; =============================================================================

;; Enable CUA mode for familiar keybindings:
;; C-c = Copy, C-x = Cut, C-v = Paste, C-z = Undo
(cua-mode 1)

;; =============================================================================
;; Emacs Server (for emacsclient)
;; =============================================================================

;; Start server if not already running
(require 'server)
(unless (server-running-p)
  (server-start))

;; When using emacsclient, don't open a new frame if one exists
(setq server-window 'pop-to-buffer)

;; =============================================================================
;; Frame Settings (for new frames via emacsclient -c)
;; =============================================================================

(defun my/frame-setup (&optional frame)
  "Configure FRAME (or current frame) with proper UI settings."
  (with-selected-frame (or frame (selected-frame))
    (when (display-graphic-p)
      (scroll-bar-mode -1))))

(my/frame-setup)
(add-hook 'after-make-frame-functions #'my/frame-setup)

;; =============================================================================
;; Package Management
;; =============================================================================

(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)

;; use-package is built into Emacs 29+
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;; =============================================================================
;; Completion Framework (Company)
;; =============================================================================

(use-package company
  :hook (after-init . global-company-mode)
  :config
  (setq company-idle-delay 0.2)
  (setq company-minimum-prefix-length 1)
  (setq company-tooltip-align-annotations t))

;; =============================================================================
;; Flycheck (Syntax Checking)
;; =============================================================================

(use-package flycheck
  :hook (after-init . global-flycheck-mode))

;; =============================================================================
;; Projectile (Project Management)
;; =============================================================================

(use-package projectile
  :config
  (projectile-mode +1)
  ;; Recognize dotnet projects by .csproj / .sln files
  (setq projectile-project-search-path '("~/projects" "~/src"))
  :bind-keymap
  ("C-c p" . projectile-command-map))

;; =============================================================================
;; Which Key (Keybinding Discoverability)
;; =============================================================================

(use-package which-key
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.5))

;; =============================================================================
;; LSP Mode (Language Server Protocol)
;; =============================================================================

(use-package lsp-mode
  :hook ((csharp-mode . lsp-deferred)
         (csharp-ts-mode . lsp-deferred)
         (web-mode . lsp-deferred))
  :commands (lsp lsp-deferred)
  :config
  (setq lsp-keymap-prefix "C-c l")
  ;; Performance tuning
  (setq lsp-idle-delay 0.5)
  (setq lsp-log-io nil)
  (setq lsp-enable-file-watchers t)
  (setq lsp-file-watch-threshold 5000)
  ;; Enable breadcrumb trail
  (setq lsp-headerline-breadcrumb-enable t)
  ;; Enable semantic tokens for richer syntax highlighting
  (setq lsp-semantic-tokens-enable t))

(use-package lsp-ui
  :after lsp-mode
  :config
  (setq lsp-ui-doc-enable t)
  (setq lsp-ui-doc-show-with-cursor t)
  (setq lsp-ui-doc-position 'at-point)
  (setq lsp-ui-sideline-enable t)
  (setq lsp-ui-sideline-show-diagnostics t)
  (setq lsp-ui-sideline-show-hover nil))

;; =============================================================================
;; C# Mode
;; =============================================================================

(use-package csharp-mode
  :mode "\\.cs\\'"
  :hook (csharp-mode . (lambda ()
                         (setq-local indent-tabs-mode nil)
                         (setq-local c-basic-offset 4)
                         (setq-local tab-width 4))))

;; =============================================================================
;; Web Mode (for Razor .cshtml files)
;; =============================================================================

(use-package web-mode
  :mode (("\\.cshtml\\'" . web-mode)
         ("\\.razor\\'" . web-mode))
  :config
  ;; Razor / ASP.NET engine
  (setq web-mode-engines-alist '(("razor" . "\\.cshtml\\'")))
  ;; Indentation
  (setq web-mode-markup-indent-offset 4)
  (setq web-mode-css-indent-offset 4)
  (setq web-mode-code-indent-offset 4)
  (setq web-mode-indent-style 4)
  ;; Auto-pairing and highlighting
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-auto-closing t)
  (setq web-mode-enable-current-element-highlight t)
  (setq web-mode-enable-current-column-highlight nil)
  ;; Enable LSP for cshtml files
  (add-to-list 'lsp-language-id-configuration '(web-mode . "csharp")))

;; =============================================================================
;; DAP Mode (Debug Adapter Protocol) -- Debugging .NET Core
;; =============================================================================

(use-package dap-mode
  :after lsp-mode
  :config
  (dap-auto-configure-mode 1)

  ;; Load the .NET Core debugger support
  (require 'dap-netcore)

  ;; Path to netcoredbg -- install with:
  ;;   https://github.com/Samsung/netcoredbg/releases
  ;; or via your package manager. Adjust the path as needed.
  (setq dap-netcore-install-dir (expand-file-name "~/.local/share/netcoredbg/"))

  ;; ------------------------------------------------------------------
  ;; Pre-configured debug templates for ASP.NET Core Razor Pages
  ;; ------------------------------------------------------------------

  ;; Launch: build and run the app, attach the debugger, open browser
  (dap-register-debug-template
   "ASP.NET Core: Launch (Razor Pages)"
   (list :type "coreclr"
         :request "launch"
         :mode "launch"
         :name "ASP.NET Core: Launch (Razor Pages)"
         :program "${workspaceFolder}/bin/Debug/net10.0/${workspaceFolderBasename}.dll"
         :cwd "${workspaceFolder}"
         :stopAtEntry :json-false
         :env '(("ASPNETCORE_ENVIRONMENT" . "Development")
                ("DOTNET_ENVIRONMENT" . "Development"))
         :args '()
         :preLaunchTask "dotnet-build"))

  ;; Attach: attach to an already-running dotnet process
  (dap-register-debug-template
   "ASP.NET Core: Attach"
   (list :type "coreclr"
         :request "attach"
         :mode "attach"
         :name "ASP.NET Core: Attach"
         :processId "${command:pickProcess}"))

  ;; ------------------------------------------------------------------
  ;; DAP UI tweaks
  ;; ------------------------------------------------------------------
  (setq dap-ui-controls-mode t)
  ;; Automatically show locals, breakpoints, etc.
  (setq dap-auto-configure-features
        '(sessions locals breakpoints expressions controls tooltip)))

;; =============================================================================
;; Dotnet Helper Functions
;; =============================================================================

(defvar my/dotnet-compilation-buffer "*dotnet*"
  "Name of the buffer for dotnet compilation output.")

(defun my/dotnet--project-root ()
  "Find the nearest directory containing a .csproj or .sln file."
  (or (locate-dominating-file default-directory
        (lambda (dir)
          (directory-files dir nil "\\.\\(csproj\\|sln\\)$")))
      (projectile-project-root)
      default-directory))

(defun my/dotnet-build ()
  "Run `dotnet build' in the project root."
  (interactive)
  (let ((default-directory (my/dotnet--project-root)))
    (compile "dotnet build" t)))

(defun my/dotnet-run ()
  "Run `dotnet run' in the project root (non-blocking)."
  (interactive)
  (let ((default-directory (my/dotnet--project-root)))
    (async-shell-command "dotnet run" my/dotnet-compilation-buffer)))

(defun my/dotnet-watch ()
  "Run `dotnet watch run' in the project root for hot-reload."
  (interactive)
  (let ((default-directory (my/dotnet--project-root)))
    (async-shell-command "dotnet watch run" "*dotnet-watch*")))

(defun my/dotnet-test ()
  "Run `dotnet test' in the project root."
  (interactive)
  (let ((default-directory (my/dotnet--project-root)))
    (compile "dotnet test" t)))

(defun my/dotnet-clean ()
  "Run `dotnet clean' in the project root."
  (interactive)
  (let ((default-directory (my/dotnet--project-root)))
    (compile "dotnet clean" t)))

(defun my/dotnet-restore ()
  "Run `dotnet restore' in the project root."
  (interactive)
  (let ((default-directory (my/dotnet--project-root)))
    (compile "dotnet restore" t)))

(defun my/dotnet-add-package (package-name)
  "Add a NuGet PACKAGE-NAME via `dotnet add package'."
  (interactive "sPackage name: ")
  (let ((default-directory (my/dotnet--project-root)))
    (compile (format "dotnet add package %s" (shell-quote-argument package-name)) t)))

(defun my/dotnet--target-framework ()
  "Read the TargetFramework from the nearest .csproj file."
  (let* ((root (my/dotnet--project-root))
         (csproj (car (directory-files root t "\\.csproj$"))))
    (when csproj
      (with-temp-buffer
        (insert-file-contents csproj)
        (when (re-search-forward "<TargetFramework>\\([^<]+\\)</TargetFramework>" nil t)
          (match-string 1))))))

(defun my/dotnet-debug ()
  "Start a DAP debug session for ASP.NET Core Razor Pages."
  (interactive)
  (require 'dap-netcore)
  ;; Build first, then launch debugger
  (let* ((default-directory (my/dotnet--project-root))
         (tfm (or (my/dotnet--target-framework) "net10.0"))
         (project-name (file-name-nondirectory
                        (directory-file-name (my/dotnet--project-root)))))
    (when (zerop (shell-command "dotnet build"))
      (dap-debug (list :type "coreclr"
                       :request "launch"
                       :mode "launch"
                       :name "ASP.NET Core Debug"
                       :program (expand-file-name
                                 (format "bin/Debug/%s/%s.dll" tfm project-name))
                       :cwd (my/dotnet--project-root)
                       :stopAtEntry :json-false
                       :env '(("ASPNETCORE_ENVIRONMENT" . "Development")
                              ("DOTNET_ENVIRONMENT" . "Development")))))))

;; =============================================================================
;; Dotnet Keybindings (C-c d prefix)
;; =============================================================================

(global-set-key (kbd "C-c d b") #'my/dotnet-build)
(global-set-key (kbd "C-c d r") #'my/dotnet-run)
(global-set-key (kbd "C-c d w") #'my/dotnet-watch)
(global-set-key (kbd "C-c d t") #'my/dotnet-test)
(global-set-key (kbd "C-c d c") #'my/dotnet-clean)
(global-set-key (kbd "C-c d R") #'my/dotnet-restore)
(global-set-key (kbd "C-c d p") #'my/dotnet-add-package)
(global-set-key (kbd "C-c d d") #'my/dotnet-debug)

;; DAP debugging keybindings (C-c D prefix)
(global-set-key (kbd "C-c D d") #'dap-debug)
(global-set-key (kbd "C-c D l") #'dap-debug-last)
(global-set-key (kbd "C-c D r") #'dap-debug-restart)
(global-set-key (kbd "C-c D q") #'dap-disconnect)
(global-set-key (kbd "C-c D b") #'dap-breakpoint-toggle)
(global-set-key (kbd "C-c D B") #'dap-breakpoint-condition)
(global-set-key (kbd "C-c D n") #'dap-next)
(global-set-key (kbd "C-c D i") #'dap-step-in)
(global-set-key (kbd "C-c D o") #'dap-step-out)
(global-set-key (kbd "C-c D c") #'dap-continue)
(global-set-key (kbd "C-c D e") #'dap-eval)
(global-set-key (kbd "C-c D E") #'dap-eval-thing-at-point)
(global-set-key (kbd "C-c D h") #'dap-hydra)

;; Quick breakpoint toggle on F9 (familiar from Visual Studio)
(global-set-key (kbd "<f9>") #'dap-breakpoint-toggle)
;; F5 to start debugging / continue
(global-set-key (kbd "<f5>") #'my/dotnet-debug)
;; Shift-F5 to stop debugging
(global-set-key (kbd "S-<f5>") #'dap-disconnect)
;; F10 to step over
(global-set-key (kbd "<f10>") #'dap-next)
;; F11 to step into
(global-set-key (kbd "<f11>") #'dap-step-in)
;; Shift-F11 to step out
(global-set-key (kbd "S-<f11>") #'dap-step-out)

;; =============================================================================
;; Magit
;; =============================================================================

(use-package magit
  :bind ("C-x g" . magit-status))

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

;; Don't show trailing whitespace
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
