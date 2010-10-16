;; Plato Wu,2010/10/16: start a project to clear up my .emacs to create a
;; robust one

;; This file only record configuration of Emacs itself. Of course it will
;; call other modules.

;; turn on font-lock mode 
;; it will let emacs'font has color.
(global-font-lock-mode t)

;; disable menu bar
(menu-bar-mode -1)

;; enable visual feedback on selections
(setq transient-mark-mode t)

;;drop startup meesage in scratch
(setq inhibit-startup-message t)
