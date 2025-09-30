;; Disabling all colors and decorations
(global-font-lock-mode -1)
(setq use-faces nil)
(dolist (face (face-list))
  (set-face-attribute face nil :foreground 'unspecified :background 'unspecified))

;; Disabling the autosave files to reduce filesystem clutter
(setq auto-save-default nil)

;; Basic UI settings
(setq inhibit-startup-screen t)
(menu-bar-mode -1)
(column-number-mode 1)
(setq battery-mode-line-format "[Battery: %b%p%% (t=%t)]")
(display-battery-mode 1)

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
    (setq indent-tabs-mode nil)))



