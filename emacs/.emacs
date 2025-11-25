(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Basic UI settings
(menu-bar-mode -1)
(column-number-mode 1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; (unless (package-installed-p 'nord-theme)
;;   (package-refresh-contents)
;;   (package-install 'nord-theme))

(unless (package-installed-p 'doric-themes)
  (package-refresh-contents)
  (package-install 'doric-themes))

;; (unless (package-installed-p 'doom-modeline)
;;   (package-refresh-contents)
;;   (package-install 'doom-modeline))

;; (unless (package-installed-p 'nerd-icons)
;;   (package-refresh-contents)
;;   (package-install 'nerd-icons))

(use-package doric-themes
  :ensure t
  :demand t
  :config
  ;; These are the default values.
  (setq doric-themes-to-toggle '(doric-dark doric-valley))
  (setq doric-themes-to-rotate '(doric-dark doric-fire doric-obsidian
					    doric-valley doric-water doric-plum doric-pine doric-mermaid))

  (doric-themes-select 'doric-mermaid)

  :bind
  (("s-<end>" . doric-themes-rotate)))

;; Mint-L settings
(set-face-background 'default "#2f2f2f")
(set-face-background 'fringe "#2f2f2f")
(setq-default mode-line-format nil)

  ;; Theme
;;(load-theme 'nord t)

;; Nerd Icons needed for Mode line
;; (use-package nerd-icons
;;   :ensure t
;;   :if (display-graphic-p)
;;   :config
;;   (unless (find-font (font-spec :name "Symbols Nerd Font Mono"))
;;     (nerd-icons-install-fonts 'yes)))

;; Fontse
(set-face-attribute 'default nil :font "Adwaita Mono")
(set-face-attribute 'variable-pitch nil :font "Adwaita Mono")
(set-face-attribute 'fixed-pitch-serif nil :font "Adwaita Mono")
(set-fontset-font t 'symbol (font-spec :family "Noto Emoji") nil 'prepend)
(set-fontset-font t 'emoji (font-spec :family "Noto Emoji") nil 'prepend)

;; ;; Mode line
;; (use-package doom-modeline
;;   :ensure t
;;   :init 
;;   (display-battery-mode 1)
;;   (doom-modeline-mode 1)
;;   :config
;;   (setq doom-modeline-height 28))

;; ;; EXWM
;; (use-package exwm
;;   :ensure t
;;   :config
;;   (add-hook 'exwm-workspace-switch-hook
;;           (lambda ()
;;             (dolist (buffer (buffer-list))
;;               (with-current-buffer buffer
;;                   (force-mode-line-update)))))

;;   (add-to-list 'exwm-manage-configurations
;;              '(".*main.*" display-buffer-same-window))
;;   (set-frame-parameter (selected-frame) 'alpha '(90 . 90))
;;   (add-to-list 'default-frame-alist '(alpha . (90 . 90)))
;;   (setq exwm-workspace-number 4)
;;   (setq exwm-input-global-keys
;; 	`(
;;           ([?\s-k] .
;; 	   (lambda ()
;; 	     (interactive)
;; 	     (exwm-workspace-switch
;; 		      (mod (1+ exwm-workspace-current-index)
;; 			   (exwm-workspace--count)))))
;; 	  ([?\s-j] .
;; 	   (lambda ()
;; 	     (interactive)
;; 	     (exwm-workspace-switch
;; 		      (mod (1- exwm-workspace-current-index)
;; 			   (exwm-workspace--count)))))
;; 	  ))
;;   (exwm-wm-mode))

;; (defun cycle-frame-alpha ()
;;   (interactive)
;;   (let* ((current (car (frame-parameter nil 'alpha)))
;;          (alphas '(50 75 90 100))
;;          (next (or (cadr (member current alphas)) (car alphas))))
;;     (dolist (frame (frame-list))
;;       (set-frame-parameter frame 'alpha `(,next . ,next)))
;;     (message "Frames alpha: %d" next)))

;; (global-set-key (kbd "s-/") 'cycle-frame-alpha)

;; ;; Increasing GC for faster Org mode startup
;; (setq gc-cons-threshold (* 50 1000 1000))

;; ;; Backups and autosaves
;; (setq backup-directory-alist
;;       `(("." . "~/.emacs.d/backups")))
;; (setq auto-save-file-name-transforms
;;       `((".*" "~/.emacs.d/auto-save/" t)))
;; (make-directory "~/.emacs.d/backups" t)
;; (make-directory "~/.emacs.d/auto-save" t)

;; Touchpad scrolling
(pixel-scroll-mode 1)
(pixel-scroll-precision-mode 1)

;; ;; Shell inside emacs
;; (global-set-key (kbd "<s-return>")
;; 		(lambda () (interactive)
;; 		  (ansi-term "/bin/bash")))
(setq shell-file-name "/bin/bash")

;; Deepseek
(unless (package-installed-p 'gptel)
  (package-refresh-contents)
  (package-install 'gptel))

;; possible workaround for UTF-8 crash on Debian
(setq gptel-use-curl t)

;; was: 'deepseek-reasoner
(setq gptel-model 'deepseek-chat
      gptel-backend (gptel-make-deepseek "DeepSeek"
                      :stream t
                      :key (with-temp-buffer
                             (insert-file-contents "~/.deepseek-secret")
                             (string-trim (buffer-string))))
      gptel-include-reasoning nil)
	    
(defun gpt-go-to-end (start end)
  (with-current-buffer "*DeepSeek*"
    (goto-char (point-max))
    ))

(add-hook 'gptel-post-response-functions 'gpt-go-to-end)


;; (defun my-compile-cmake ()
;;   "Run CMake build and open app in top-right window."
;;   (interactive)
;;   ;; Ensure top-right window exists and focus it temporarily
;;   (let ((target-window (or (get-buffer-window "*compilation*")
;;                           (progn
;;                             (delete-other-windows)
;;                             (split-window-horizontally)
;;                             (other-window 1)
;;                             (split-window-vertically)
;;                             (selected-window)

;; 			    ))))
;;     (select-window target-window)
;;     (compile "cmake --build build -j && ./build/main")
;;     ;; Return focus to where you were
;;     (other-window -1)))



;; this kinda works if we already have a split
;; and the default-directory of the top right is correct
(defun my-compile-cmake ()
  "Run CMake build in right window."
  (interactive)
  (let ((compilation-always-kill t))
  ;; Create right window if it doesn't exist
  ;; (unless (and (one-window-p) (window-parent))
  ;;   (delete-other-windows))
  ;; (when (one-window-p)
    ;;   (split-window-horizontally))
  
  ;; Run in right window
  (let ((current (selected-window)))
    (select-window (next-window))
    (compile "cmake --build build -j && ./build/main")
    ;(select-window current)

    ;; TODO: it would be cool to return to the cpp window BUT
    ;; only after EXWM opens up the window!!
    
    )))  ; Return to left window

;; Bind to F5
(global-set-key (kbd "C-c C-r") 'my-compile-cmake)

;; C/C++ formatting settings
(add-hook 'c-mode-common-hook
	  (lambda ()
                    	    (setq c-basic-offset 4)
	    (setq tab-width 4)
	    (setq indent-tabs-mode nil)
;	    (setq font-lock-maximum-decoration 1)
	    (local-set-key (kbd "C-c o") 'ff-find-other-file)
	    ))

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; Useful macros
(defalias 'copy-para-to-other-window
   (kmacro "M-h M-w C-x o M-> C-y")) ; for script writing with AI

(global-set-key (kbd "C-c p") 'copy-para-to-other-window)

;; Org Mode
(use-package org
  :config
  (set-face-attribute 'org-ellipsis nil :underline nil)
  (setq org-ellipsis  " ▼"
	org-hide-emphasis-markers t
	org-clock-mode-line-total 'today
        org-duration-format (quote h:mm))
  (add-hook 'org-mode-hook (lambda ()
			     (org-indent-mode)
			     (variable-pitch-mode -1)
			     (auto-fill-mode 1))))
  
(defun ffplay-media-url ()
  "Open media URL at point with ffplay"
  (interactive)
  (let ((url (get-text-property (point) 'help-echo)))
    (when url
      (async-shell-command 
       (format "ffplay -autoexit '%s'" url)))))


;; ---------------------
;; Emacs-generated stuff
;; ---------------------
