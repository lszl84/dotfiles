;;; init.el --- minimal setup -*- lexical-binding: t -*-

(cua-mode 1)
(load-theme 'modus-vivendi t)
(tool-bar-mode 1)
(fringe-mode 0)
(setq-default mode-line-format nil)

;; org-mode: indentation replaces stars, Unicode bullets
(add-hook 'org-mode-hook #'org-indent-mode)
(add-hook 'org-mode-hook #'auto-fill-mode)
(setq org-hide-emphasis-markers t)
