;; turn on font-lock mode 
;; it will let emacs'font has color.
(global-font-lock-mode t)

;; disable menu bar
(menu-bar-mode -1)

;; enable visual feedback on selections
(setq transient-mark-mode t)

;;drop startup meesage in scratch
(setq inhibit-startup-message t)

;; display another parentheses not jump to it
(show-paren-mode t)
(setq show-paren-style 'parentheses)

(try-function toggle-scroll-bar -1)

(try-function tool-bar-mode -1)

;;Display date
(setq display-time-day-and-date t)

(display-time-mode 1)
;;it let M-j start a new comment line, it is useful
;;for language which just have line comment operator.
(setq comment-multi-line nil)

;;Toggle incremental minibuffer completion.
(icomplete-mode 1)

; allow the minibuffer to be resized when it needs the space for a long message
(setq resize-minibuffer-mode t)

;;All question are asked by y/n, not yes/no
(fset 'yes-or-no-p 'y-or-n-p)

; can use minibuffers recursively
(setq enable-recursive-minibuffers t)
; allow the minibuffer to be resized when it needs the space for a long message
(setq resize-minibuffer-mode t)

;;set latitude & longitude of location(Shanghai)
(setq calendar-latitude +31.14)
(setq calendar-longitude +121.29)
;;set location name
(setq calendar-location-name "Shanghai")
;;set first day of week is Monday
(setq calendar-week-start-day 1)

;;Delete Christian,Hebrew,Islam's holidays
(setq christian-holidays nil)
(setq hebrew-holidays nil)
(setq islamic-holidays nil)
;;Delete holidays in Unite State
(setq general-holidays nil)

;;mark holidays in calendar
(setq mark-holidays-in-calendar t)
;;auto open holidays & birthday list 
(setq view-calendar-holidays-initially t)

;;let search case sensitively
(setq-default case-fold-search nil)

;; Plato Wu,2009/11/26: all indentation must be made from spaces only.
(setq-default indent-tabs-mode nil)

(setq vc-follow-symlinks t)

;; Plato Wu,2010/07/27: Add tags-file-name
(setq desktop-locals-to-save (append desktop-locals-to-save '(buffer-display-time tags-file-name)))
(desktop-save-mode 1)

(provide 'my-interface)


