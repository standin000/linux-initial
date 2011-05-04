;; Plato Wu,2009/11/29: ispell-word need aspell and aspell-en get installed.

;; uncomment this line to disable loading of "default.el" at startup
;; (setq inhibit-default-init t)

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

(try-function '(toggle-scroll-bar -1))

(try-function '(tool-bar-mode -1))

;;Display date
(setq display-time-day-and-date t)

;; Plato Wu,2011/04/09: don't display time for there is bmpanel2 
(display-time-mode 0)
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

;;set latitude & longitude of location(Shanghai)
(setq calendar-latitude +31.14)
(setq calendar-longitude +121.29)
;;set location name
(setq calendar-location-name "Shanghai")
;;set first day of week is Monday
(setq calendar-week-start-day 1)

;; Plato Wu,2009/12/25: emacs 23 in cygwin does know the correct time zone.
(if (and (is-system "cygwin") (is-version 23))
 (set-time-zone-rule "GMT-8"))

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

;; Plato Wu,2008/11/20, It is not good for I use desktop for save all buffers;
;; it will update all buffer's visit date when open emacs.so it need modify
;; this variables
;; Plato Wu,2010/07/27: Add tags-file-name
(setq desktop-locals-to-save (append desktop-locals-to-save '(buffer-display-time tags-file-name)))

(setq desktop-buffers-not-to-save '("*Music*"))

(setq scroll-margin 3
       scroll-conservatively 10000)

(cond 
 ((is-version 21)
  (progn
   ;; Plato Wu,2009/02/27: desktop-load-default is obsolete since 22.1
   ;; use desktop-save-mode
   (desktop-load-default) 
   (desktop-read)
   (unless (file-readable-p "~/.emacs.desktop")
     (desktop-save "~/"))))
 ((is-system "windows-nt")
  (desktop-read "~/w32.emacs"))
 (t (desktop-save-mode 1)))

(add-hook 'desktop-after-read-hook 'clean-buffer-list)

;;set personal information
(setq user-full-name "Plato Wu")
(setq user-mail-address "gtalk000@gmail.com")

;;Non-nil means cutting and pasting uses the clipboard.
(setq x-select-enable-clipboard t)

;;set % like vi
(global-set-key "%" 'match-parenthesis)

(defun match-parenthesis (arg)
  "Go to the matching parenthesis if on a parenthesis; otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) 
;;08/08/05, Plato: remarks for using copy region situation.
	 ;; (backward-char 1)
	 )
	((looking-at "\\s\)") (forward-char 1) 
	 (backward-list 1))
	(t (self-insert-command (or arg 1)))))

;;move mouse point when cursor is closing it
;; mouse avoidance animate mode is ng.
(if (display-mouse-p)
    (mouse-avoidance-mode 'jump))

(when window-system 
  ;;auto open & display image
  (auto-image-file-mode)
  (pc-selection-mode)			; use shift to select region
  (setq pc-select-selection-keys-only t)
  (cond 
   ((is-system "darwin")
    (set-default-font "-apple-courier-medium-r-normal--18-180-72-72-m-180-iso10646-1"))
   ((is-system "windows-nt")
    nil)
   (t (progn 
        (create-fontset-from-fontset-spec 
         (concat
          "-*-fixed-*-*-*-*-*-200-*-*-*-*-fontset-courier," 
          "chinese-gb2312:-ISAS-Fangsong ti-Medium-R-Normal--16-*-*-*-c-*-GB2312*-*,"))
        (set-default-font "fontset-courier")
        (setq default-frame-alist
              (append
               '((font . "fontset-courier")) default-frame-alist))))))

(setq debug-on-error t)

(require 'savehist)

(savehist-mode 1)

;;; M-q to auto fill paragraph
;(setq fill-column 70)

;;display column number
(setq column-number-mode t)

;;set dired mode to operation recursively
(setq dired-recursive-copies 'top)
(setq dired-recursive-deletes 'top)

;; Plato Wu,2010/01/26: Start to use shell mode instead of window in screen.
;; Let shell don't query when exit emacs.
;; Plato Wu,2009/04/10: Do not use shell and it seems it always show color
;; in emacs 22.2.1
;; let shell mode can show color
;; (autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
;; (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-hook 'shell-mode-hook
          (lambda ()
            (set-process-query-on-exit-flag
              (get-buffer-process (current-buffer)) nil))) 

(global-auto-revert-mode 1)

(when (is-system "cygwin")
  ;; Plato Wu,2010/03/29: emacs in cygwin does not support . of num area key
  (global-set-key "On" 
                  '(lambda () 
                     (interactive)
                     (insert ".")))
  (defvar mode-line-position nil)
  (let ((help-echo "mouse-1: select (drag to resize), mouse-2 = C-x 1, mouse-3 = C-x 0"))
    (setq mode-line-position `((-3 ,(propertize "%p" 'help-echo help-echo))
			      (size-indication-mode
			       (8 ,(propertize " of %I" 'help-echo help-echo)))
			      (line-number-mode
			       ((column-number-mode
				 (10 ,(propertize " (%l,%c)" 'help-echo help-echo))
				 (6 ,(propertize " L%l" 'help-echo help-echo))))
			       ((column-number-mode
				 (5 ,(propertize " C%c" 'help-echo help-echo)))))))))

(setq-default 
 mode-line-format
 '("%e"
   #("-" 0 1
     (help-echo "mouse-1: select (drag to resize), mouse-2 = C-x 1, mouse-3 = C-x 0"))
   ;; Plato Wu,2009/07/27: It seems meaningless
   ;;       mode-line-mule-info 
   mode-line-modified 
   ;; Plato Wu,2009/07/27: It seems meaningless
   ;;       mode-line-frame-identification 
   mode-line-buffer-identification
   #("   " 0 3
     (help-echo "mouse-1: select (drag to resize), mouse-2 = C-x 1, mouse-3 = C-x 0"))
   mode-line-position
   (vc-mode vc-mode)
   #("  " 0 2
     (help-echo "mouse-1: select (drag to resize), mouse-2 = C-x 1, mouse-3 = C-x 0"))
   ;; Plato Wu,2009/07/27: Disable it for it is very long sometime
   ;;       mode-line-modes
   (which-func-mode
    ;; Plato Wu,2009/07/27: "" is need for symbol in list in mode line.
    ("" which-func-format
     #("--" 0 2
       (help-echo "mouse-1: select (drag to resize), mouse-2 = C-x 1, mouse-3 = C-x 0"))))
   (global-mode-string
    (#("--" 0 2
       (help-echo "mouse-1: select (drag to resize), mouse-2 = C-x 1, mouse-3 = C-x 0"))
     global-mode-string))
   #("-%-" 0 3
     (help-echo "mouse-1: select (drag to resize), mouse-2 = C-x 1, mouse-3 = C-x 0"))))

(when window-system; or (boundp 'x-display-name)
  (cond 
   ((string= system-type "darwin")
      (set-default-font "-apple-courier-medium-r-normal--18-180-72-72-m-180-iso10646-1"))
   ((string= system-type "windows-nt")
      ;(set-default-font "-apple-courier-medium-r-normal--18-180-72-72-m-180-iso10646-1")
      nil)
   (t (progn 
      (create-fontset-from-fontset-spec 
       (concat
	"-*-fixed-*-*-*-*-*-200-*-*-*-*-fontset-courier," 
	"chinese-gb2312:-ISAS-Fangsong ti-Medium-R-Normal--16-*-*-*-c-*-GB2312*-*,"))
      (set-default-font "fontset-courier")
      (setq default-frame-alist
	        (append
		      '((font . "fontset-courier")) default-frame-alist))))))

(when window-system 
  (defun toggle-fullscreen ()
    "Toggle between fullscreen and partial screen display on X11;
       courtesy of http://www.emacswiki.org/cgi-bin/wiki/FullScreen"
    (interactive)
    (if (is-system "windows-nt")
        (w32-send-sys-command 61488)
      (x-send-client-message nil 0 nil "_NET_WM_STATE" 32 '(2 "_NET_WM_STATE_FULLSCREEN" 0)))
    ;;    (work-frame-defaults)
    (put 'current-frame 'frame-mode 'fullscreen))
  ;; Plato Wu,2010/04/01: Now F9 is assigned into next-error
;  (global-set-key [f9] 'toggle-fullscreen)
  ;; Plato Wu,2011/04/12: Now I have panel, so don't toggle fullscreen
;  (toggle-fullscreen)
  )

;; Plato Wu,2010/07/13: make tags-file-name buffer local
(make-variable-buffer-local 'tags-file-name)
(make-variable-buffer-local 'tags-table-list)
;;add a new tags table to the current list.
(setq tags-add-tables t)
(put 'tags-file-name 'safe-local-variable 'stringp)

; prevent emacs from truncating  message in the echo area.
(setq eval-expression-print-length nil)

(defun goto-last-edit-point ()
  "Go to the last point where editing occurred."
  (interactive)
  (let ((undos buffer-undo-list))
    (when (listp undos)
      (while (and undos
                  (let ((pos (or (cdr-safe (car undos))
                                 (car undos))))
                    (not (and (integerp pos)
                              (goto-char (abs pos))))))
        (setq undos (cdr undos))))))

(global-set-key (kbd "C-c SPC") 'goto-last-edit-point)

;; Plato Wu,2010/09/01: This pushes the backup files into an invisible
;; directory named .~ in the directory of the corresponding file. 
(setq backup-directory-alist '(("." . ".~")))

;; Plato Wu,2010/10/10: let the last incremental search
;; string is used as FROM-STRING in query-replace
;; Plato Wu,2010/10/14: inconvient
(setq query-replace-interactive nil)

;;=============
;; Plato Wu,2009/11/23: In Windows, Most of source code file are used with gb2312
;; (if (string= system-type "windows-nt")
;;     (prefer-coding-system 'gb2312))
;;==================

;; (prefer-coding-system 'utf-8)
;; Plato Wu,2008/09/27, if lang is not zh_CN,
;; only set language-evnrionment to Chinese-GB or UTF-8 is NG.
;;===================
;; Chinese-GB will cause \xxx and UTF-8 will cause japanese char
;; (set-language-environment 'Chinese-GB)
;; (set-language-environment 'UTF-8))
;; (when (string= system-type "cygwin")
;; ;; Plato Wu,2009/10/15: make emacs show Chinese.
;; Plato Wu,2010/05/30: It seems Kitty doesn't need these
;;   (set-language-environment 'Chinese-GB)
;;   (set-terminal-coding-system 'gb2312-dos)
;;   ;; Plato Wu,2010/03/25: It has to set coding system for TAGS
;;   (modify-coding-system-alist 'file "TAGS\\'" 'chinese-iso-8bit-dos))
;;   
;;==================

(provide 'my-interface)

;; (defun my-search (regexp)
;;   (interactive
;;    (let ((regexp (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
;;      (grep-compute-defaults)
;;      (list regexp)))
;;   (rgrep regexp "*.el" "~/linux-initial/emacs/" nil))

;; (global-set-key [f9] 'my-search)
(setq grep-files-aliases
      '(("asm" .    "*.[sS]")
        ;; Plato Wu,2010/02/27: Let ch match .c file first!
        ("ch" .    "*.[ch]")
        ("c" .     "*.c")
        ("cc" .    "*.cc *.cxx *.cpp *.C *.CC *.c++")
        ("cchh" .    "*.cc *.[ch]xx *.[ch]pp *.[CHh] *.CC *.HH *.[ch]++")
        ("hh" .    "*.hxx *.hpp *.[Hh] *.HH *.h++")
        ("el" .    "*.el")
        ("h" .     "*.h")
        ("l" .      "[Cc]hange[Ll]og*")
        ("m" .     "[Mm]akefile*")
        ("tex" .    "*.tex")
        ("texi" .   "*.texi")))

;;C-x ( start-kbd-macro; C-x ) end-kbd-macro; C-x e call-last-kbd-macro
;; Dos to unix
;; M-x set-buffer-file-coding-system RET undecided-unix save the file (C-x C-s)
;; or
;; C-x RET f undecided-unix C-x C-f
;; Unix to dos
;; M-x set-buffer-file-coding-system RET undecided-dos save the file (C-x C-s)
;; or
;; C-x RET f undecided-dos C-x C-f

;; Plato Wu,2011/04/15: after add XTerm*allowSendEvents: True into .Xresourcs
;; (frame-parameter (selected-frame) 'background-color) is unspecified-bg in
;; xterm, why? urxvt is OK.


;; (set-face-foreground 'default "wheat")
;; (set-face-background 'default "darkslategrey")
;; (set-foreground-color "wheat")



;; Plato Wu,2011/04/27: use black like ssh tools: kitty, so red comment is OK.
(set-background-color "darkslategrey")

; show the current function when possible; it need cedet to know function'name
(which-func-mode 1)





