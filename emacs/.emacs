;; Prevent white flash on load
(setq frame-background-mode 'dark)

;; Basic UI settings
(menu-bar-mode -1)
(column-number-mode 1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; doric-themes
(unless (package-installed-p 'doric-themes)
  (package-refresh-contents)
  (package-install 'doric-themes))

(use-package doric-themes
  :ensure t
  :init
  :config
  (doric-themes-select 'doric-fire))

;; EXWM
(use-package exwm
  :ensure t
  :config
  (exwm-wm-mode))

;; Font size
(set-frame-font "Monospace 14")
(set-face-attribute 'variable-pitch nil :height 140)
(set-fontset-font t 'symbol (font-spec :family "Noto Emoji") nil 'prepend)
(set-fontset-font t 'emoji (font-spec :family "Noto Emoji") nil 'prepend)

;; Increasing GC for faster Org mode startup
(setq gc-cons-threshold (* 50 1000 1000))

;; Backups and autosaves
(setq backup-directory-alist
      `(("." . "~/.emacs.d/backups")))
(setq auto-save-file-name-transforms
      `((".*" "~/.emacs.d/auto-save/" t)))
(make-directory "~/.emacs.d/backups" t)
(make-directory "~/.emacs.d/auto-save" t)

;; Modeline Battery
(setq battery-mode-line-format " 🔋%b%p%% (%t)")
(display-battery-mode 1)

;; Custom modeline with RAM usage
;; Cache variable for memory usage
(defvar my-memory-usage-cache "0B")

;; Function to update memory usage
(defun my-update-memory-usage ()
  (ignore-errors
    (setq my-memory-usage-cache
          (string-trim
           (shell-command-to-string 
            "free -h | awk 'NR==2{print $3}'")))))

;; Start timer to update every 20 seconds
(run-with-timer 0 20 'my-update-memory-usage)

;; Initial update
(my-update-memory-usage)

;; Mode line format
(setq-default mode-line-format
  '("%e" mode-line-front-space
    mode-line-mule-info
    mode-line-client
    mode-line-modified
    mode-line-remote
    mode-line-frame-identification
    mode-line-buffer-identification
    " "
    mode-line-position
    " "
    (:eval (concat " 🗄️" my-memory-usage-cache))
    mode-line-misc-info
    mode-line-end-spaces))

;; Touchpad scrolling
(pixel-scroll-mode 1)
(pixel-scroll-precision-mode 1)

;; M-x man
(setq Man-notify-method 'bully) 
(setq Man-width 'maximum)
(setq Man-width-max 100) 

;; C-c m - to open man at point
;; Define the function
(defun man-dwim ()
  "Open man page for symbol at point in current window."
  (interactive)
  (let ((Man-notify-method 'bully)
	)
    (man (current-word))))

;; Then bind the key
(global-set-key (kbd "C-c m") 'man-dwim)

;; Shell inside emacs
(global-set-key (kbd "<s-return>")
		(lambda () (interactive)
		  (ansi-term "/bin/bash")))
(setq shell-file-name "/bin/bash")
(global-set-key (kbd "s-b") 'switch-to-buffer)
(global-set-key (kbd "s-o") 'other-window)

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

(defun my-compile-cmake ()
  "Run CMake build and replace compilation buffer with app output on success."
  (interactive)
  (compile "cmake --build build -j && ./build/main")

)

;; Bind to F5
(global-set-key (kbd "C-c C-r") 'my-compile-cmake)

;; C/C++ formatting settings
(add-hook 'c-mode-common-hook
	  (lambda ()
	    (setq c-basic-offset 4)
	    (setq tab-width 4)
	    (setq indent-tabs-mode nil)
	    (setq font-lock-maximum-decoration 1)
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
  (setq org-ellipsis  " ▼"
	org-hide-emphasis-markers t)
  (add-hook 'org-mode-hook (lambda ()
			     (org-indent-mode)
			     (variable-pitch-mode -1)
			     (auto-fill-mode 1))))
  

;; ---------------------
;; Emacs-generated stuff
;; ---------------------
