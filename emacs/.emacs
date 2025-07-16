
(package-initialize)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Enable built-in package manager (no need for MELPA in Emacs 29+)


;; Colors, fonts
(use-package doric-themes
  :ensure t
  :demand t
  :config
  ;; These are the default values.
  (setq doric-themes-to-toggle '(doric-light doric-dark))
  (setq doric-themes-to-rotate doric-themes-collection)

  (doric-themes-select 'doric-dark)
)
(set-face-attribute 'default nil :family "Monospace" :height 120)

(use-package treemacs
  :ensure t
  :defer t  ; Load only when invoked
  :config
  ;; Optional: Auto-follow current file
  (treemacs-follow-mode t))
(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))


;; Install eglot if missing
(unless (package-installed-p 'eglot)
  (package-refresh-contents)
  (package-install 'eglot))

;; Configure eglot for C++
(add-hook 'c++-mode-hook 'eglot-ensure)
(setq eglot-autoshutdown t)

;; Set up Wombat theme (built-in in modern Emacs)
;(load-theme 'wombat t)
(tool-bar-mode -1)   ; Optional: Also remove toolbar
(menu-bar-mode 1)   ; Optional: Remove menu bar
(scroll-bar-mode -1) ; Hide scrollbar

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
                         "cmake -S. -Bbuild -DCMAKE_BUILD_TYPE=Debug "
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
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
