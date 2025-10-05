;; Disabling all colors and decorations
;; (global-font-lock-mode -1) ; I guess this is not necessary?
			      ; manpages look nice with formatting

;; Except Modeline
(set-face-attribute 'mode-line nil 
                    :foreground (face-attribute 'default :background) 
                    :background (face-attribute 'default :foreground))
(set-face-attribute 'mode-line-inactive nil 
                    :foreground (face-attribute 'default :background) 
                    :background (face-attribute 'default :foreground))

;; THEN clear other faces 
(dolist (face (face-list))
  (unless (memq face '(mode-line mode-line-inactive))
    (set-face-attribute face nil :foreground 'unspecified :background 'unspecified)))


;; Increasing GC for faster Org mode startup
(setq gc-cons-threshold (* 50 1000 1000))

;; Backups and autosaves
(setq backup-directory-alist `(("." . "~/.emacs.d/backups")))
(setq auto-save-file-name-transforms `((".*" "~/.emacs.d/auto-save/" t)))
(make-directory "~/.emacs.d/backups" t)
(make-directory "~/.emacs.d/auto-save" t)

;; Basic UI settings
(setq inhibit-startup-screen t)
(menu-bar-mode -1)
(column-number-mode 1)
(setq battery-mode-line-format "[Battery: %b%p%% (t=%t)]")
(display-battery-mode 1)

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


;; ----------------------- TODO: Deepseek
;; gptel-include-reasoning nil

(unless (package-installed-p 'gptel)
  (package-refresh-contents)
  (package-install 'gptel))

(setq gptel-model 'deepseek-reasoner
      gptel-backend (gptel-make-deepseek "DeepSeek"
                      :stream t
                      :key (with-temp-buffer
                             (insert-file-contents "~/.deepseek-secret")
                             (string-trim (buffer-string))))
      gptel-include-reasoning nil)


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
(global-set-key (kbd "<f5>") 'my-compile-cmake)

;; C/C++ formatting settings
(add-hook 'c-mode-common-hook
	  (lambda ()
	    (setq c-basic-offset 4)
	    (setq tab-width 4)
	    (setq indent-tabs-mode nil)
	    (local-set-key (kbd "C-c o") 'ff-find-other-file)
	    ))

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
