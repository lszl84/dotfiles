(global-font-lock-mode -1)
(setq use-faces nil)
(dolist (face (face-list))
  (set-face-attribute face nil :foreground 'unspecified :background 'unspecified))

(setq inhibit-startup-screen t)
(menu-bar-mode -1)

