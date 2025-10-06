;; Basic Monochrome
(set-face-attribute 'default nil 
                    :foreground "#d3c6aa" 
                    :background "black")

;; Comments - slightly darker
(set-face-attribute 'font-lock-comment-face nil
                    :foreground "#a6a08c")

;; Modeline
(set-face-attribute 'mode-line nil 
                    :foreground (face-attribute 'default :background) 
                    :background (face-attribute 'default :foreground))
(set-face-attribute 'mode-line-inactive nil 
                    :foreground (face-attribute 'default :background) 
                    :background (face-attribute 'default :foreground))

;; Clear other faces
(dolist (face (face-list))
  (unless (memq face '(mode-line mode-line-inactive default
                      font-lock-comment-face 
        ))
    (set-face-attribute face nil
                        :foreground 'unspecified
                        :background 'unspecified)))

;; Font size
(set-frame-font "Monospace 15")

;; Increasing GC for faster Org mode startup
(setq gc-cons-threshold (* 50 1000 1000))

;; Backups and autosaves
(setq backup-directory-alist
      `(("." . "~/.emacs.d/backups")))
(setq auto-save-file-name-transforms
      `((".*" "~/.emacs.d/auto-save/" t)))
(make-directory "~/.emacs.d/backups" t)
(make-directory "~/.emacs.d/auto-save" t)

;; Basic UI settings
(menu-bar-mode -1)
(column-number-mode 1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

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
(global-set-key (kbd "<s-return>") 'shell)

;; Deepseek
(unless (package-installed-p 'gptel)
  (package-refresh-contents)
  (package-install 'gptel))

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

;; Simple C++ compile with auto-hiding the compilation window if no errors
(defun my-compile-cmake ()
  "Run CMake build and auto-close compile window on success."
  (interactive)
  (compile "cmake --build build -j && ./build/main")
  (setq compilation-exit-message-function
        (lambda (process-status exit-code msg)
          (when (and (eq process-status 'exit) (zerop exit-code))
            (delete-window (get-buffer-window "*compilation*")))
          (cons msg exit-code))))

;; Bind to F5
(global-set-key (kbd "C-c C-r") 'my-compile-cmake)

;; C/C++ formatting settings
(add-hook 'c-mode-common-hook
	  (lambda ()
	    (setq c-basic-offset 4)
	    (setq tab-width 4)
	    (setq indent-tabs-mode nil)
	    (local-set-key (kbd "C-c o") 'ff-find-other-file)
	    ))

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; Useful macros
(defalias 'copy-para-to-other-window
   (kmacro "M-h M-w C-x o M-> C-y")) ; for script writing with AI

;; ---------------------
;; Emacs-generated stuff
;; ---------------------

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
