;; Basic UI settings
(menu-bar-mode -1)
(column-number-mode 1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(unless (package-installed-p 'doric-themes)
  (package-refresh-contents)
  (package-install 'doric-themes))

(load-theme 'doric-dark t)
 
;; EXWM
(use-package exwm
  :ensure t
  :config
  (add-hook 'exwm-workspace-switch-hook
          (lambda ()
            (dolist (buffer (buffer-list))
              (with-current-buffer buffer
                  (force-mode-line-update)))))

  (add-to-list 'exwm-manage-configurations
             '(".*main.*" display-buffer-same-window))
  (setq exwm-workspace-number 2)
  (setq exwm-input-global-keys
	`(
          ([?\s-o] .
	   (lambda ()
	     (interactive)
	     (exwm-workspace-switch
		      (mod (1+ exwm-workspace-current-index)
			   (exwm-workspace--count)))))
	  ))
  (exwm-wm-mode))


;; Font size
(set-face-attribute 'default nil
		    :font "-xos4-terminus-medium-r-normal--20-200-72-72-c-100-iso10646-1")
(set-face-attribute 'variable-pitch nil
		    :font "-xos4-terminus-medium-r-normal--20-200-72-72-c-100-iso10646-1")
(set-face-attribute 'fixed-pitch-serif nil
		    :font "-xos4-terminus-bold-r-normal--20-200-72-72-c-100-iso10646-1")
(set-fontset-font t 'symbol (font-spec :family "Noto Emoji") nil 'prepend)
(set-fontset-font t 'emoji (font-spec :family "Noto Emoji") nil 'prepend)

(defvar my-terminus-fonts
  '("-xos4-terminus-medium-r-normal--20-200-72-72-c-100-iso10646-1"
    "-xos4-terminus-medium-r-normal--22-220-72-72-c-110-iso10646-1" 
    "-xos4-terminus-medium-r-normal--24-240-72-72-c-120-iso10646-1"
    "-xos4-terminus-medium-r-normal--28-280-72-72-c-140-iso10646-1"
    "-xos4-terminus-medium-r-normal--32-320-72-72-c-160-iso10646-1")
  "List of Terminus fonts to cycle through.")

(defvar my-terminus-font-index 0
  "Current index in the Terminus fonts list.")

(defun my-cycle-terminus-font ()
  "Cycle through predefined Terminus fonts."
  (interactive)
  (setq my-terminus-font-index 
        (mod (1+ my-terminus-font-index) (length my-terminus-fonts)))
  (let ((font (nth my-terminus-font-index my-terminus-fonts)))
    (set-face-attribute 'default nil :font font)
    (set-face-attribute 'variable-pitch nil :font font)
    (message "Terminus font: %s" font)))

;; Bind to a key, e.g. C-c f
(global-set-key (kbd "C-c f") 'my-cycle-terminus-font)


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
    " "
    mode-line-buffer-identification
    " "
    mode-line-position
    " "
    (:eval (format " 💻%d" exwm-workspace-current-index))
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
	org-clock-mode-line-total 'today)
  (add-hook 'org-mode-hook (lambda ()
			     (org-indent-mode)
			     (variable-pitch-mode -1)
			     (auto-fill-mode 1))))
  



;; ---------------------
;; Emacs-generated stuff
;; ---------------------
