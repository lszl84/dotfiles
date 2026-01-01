;;; early-init.el --- Early initialization -*- lexical-binding: t -*-

;;; Commentary:
;; This file is loaded before the package system and GUI are initialized.
;; It's the ideal place to configure UI elements to prevent flicker.

;;; Code:

;; Disable UI elements early to prevent momentary display
(push '(menu-bar-lines . 0) default-frame-alist)  ; No menu bar
(push '(tool-bar-lines . 0) default-frame-alist)  ; No tool bar
(push '(vertical-scroll-bars) default-frame-alist) ; No scroll bars

;; Set dark background early to prevent white flash
(push '(background-color . "#212121") default-frame-alist)
(push '(foreground-color . "#ffffff") default-frame-alist)

;; Prevent package.el from auto-loading packages at startup
;; (if you plan to use use-package with straight.el or similar)
;; (setq package-enable-at-startup nil)

;; Increase GC threshold during startup for faster loading
(setq gc-cons-threshold most-positive-fixnum)

;; Prevent unwanted runtime compilation for gccemacs
(setq native-comp-deferred-compilation nil)
(setq native-comp-async-report-warnings-errors 'silent)

;;; early-init.el ends here

