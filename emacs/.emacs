(package-initialize)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(pixel-scroll-precision-mode 1)

;; Colors, fonts
(use-package doric-themes
  :ensure t
  :config
   (doric-themes-select 'doric-obsidian))

(set-face-attribute 'default nil :family "Monospace" :height 120)
(set-face-foreground 'vertical-border "gray")
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fringe ((t (:background nil)))))

(use-package treemacs
  :ensure t
  :defer t  ; Load only when invoked
  :config
  ;; Optional: Auto-follow current file
  (setq treemacs-width-is-initially-locked nil)
(define-key treemacs-mode-map [mouse-1] #'treemacs-single-click-expand-action)
  (treemacs-follow-mode t))
(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))

(defun my/set-treemacs-variable-width-font ()
  "Force variable-width font for all Treemacs text elements."
  (interactive)
  (let ((font-family "DejaVu Sans"))  ; Replace with your font name
    (dolist (face '(treemacs-directory-face
                    treemacs-directory-collapsed-face
                    treemacs-file-face
                    treemacs-root-face
                    treemacs-root-unreadable-face
                    treemacs-root-remote-face
                    treemacs-root-remote-unreadable-face
                    treemacs-root-remote-disconnected-face
                    treemacs-tags-face
                    treemacs-help-title-face
                    treemacs-help-column-face
                    treemacs-term-node-face
                    treemacs-header-button-face
                    treemacs-git-commit-diff-face
                    treemacs-git-added-face
                    treemacs-git-conflict-face
                    treemacs-git-ignored-face
                    treemacs-git-modified-face
                    treemacs-git-renamed-face
                    treemacs-git-untracked-face
                    ))
      (set-face-attribute face nil :family font-family :height 1.0)
      ;; Clear any inheritance that might force monospace
      (set-face-attribute face nil :inherit nil))
    
    ;; Special case for hl-line to avoid affecting the background
    (set-face-attribute 'treemacs-hl-line-face nil :family font-family :inherit 'hl-line)
    
    ;; Make sure the default face in treemacs uses our font
    (set-face-attribute 'treemacs-window-background-face nil :family font-family :inherit 'default)))

(with-eval-after-load 'treemacs
  (my/set-treemacs-variable-width-font))


(tool-bar-mode -1)   ; Optional: Also remove toolbar
(menu-bar-mode -1)   ; Optional: Remove menu bar
(scroll-bar-mode -1) ; Hide scrollbar

(setq-default cursor-type 'bar)

;; Function to find project root (both CMakeLists.txt and .git)
(defun find-project-root (start-dir)
  "Find the nearest parent directory containing both CMakeLists.txt and .git"
  (let ((dir (locate-dominating-file start-dir
                   (lambda (dir)
                     (and (file-exists-p (expand-file-name "CMakeLists.txt" dir))
                          (file-exists-p (expand-file-name ".git" dir)))))))
    (if dir (expand-file-name dir) nil)))


(defun set-project-root-as-default-directory ()
  "Set default-directory to project root when opening a file"
  (when-let* ((root (find-project-root (or (buffer-file-name) default-directory))))
    (setq default-directory root)))

;; Hook to run when opening files
(add-hook 'find-file-hook 'set-project-root-as-default-directory)

;; Function to build and run project
(defun build-and-run-project ()
  "Find project root and run cmake build commands"
  (interactive)
  (let* ((current-file (or (buffer-file-name) default-directory))
         (project-root (find-project-root (file-name-directory current-file))))
    (if project-root
        (let ((default-directory project-root))
          (compile (concat "cd " project-root " && "
                         "CC=clang CXX=clang++ cmake -S. -Bbuild -DCMAKE_BUILD_TYPE=Debug "
                         "-DCMAKE_EXPORT_COMPILE_COMMANDS=1 -GNinja && "
                         "cmake --build build -j && "
                         "build/main")))
      (message "Could not find project root (needs both CMakeLists.txt and .git)"))))

;; Set F5 keybinding
(global-set-key (kbd "<f5>") 'build-and-run-project)

;; Basic C++ setup
(add-hook 'c++-mode-hook
          (lambda ()
            (setq c-basic-offset 4)
            (setq tab-width 4)
            (setq indent-tabs-mode nil)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil))

