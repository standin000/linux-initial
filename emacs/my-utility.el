(defmacro try-function (function)
  `(if (functionp (car ,function))
      (eval ,function)
    'NG))

(defmacro try-require (library)
  `(try-function '(require ,library)))

(defmacro try-require (library)
  (try-function require library))


(defun is-system (type)
  (string= system-type type))

(defun is-version (no)
  (string= (substring emacs-version 0 2) (number-to-string no)))

(defun mycomment (arg)
  (interactive "*P")
  (comment-dwim arg)
  (insert (user-full-name) (format-time-string ",%Y/%m/%d: " (current-time))))


;; Plato Wu,2010/04/14: tramp useage
;; /ssh:username@host:/path
;; it can not work in this version of tramp
;; (add-to-list 'tramp-default-proxies-alist
;; 	     '("remote_hostname" "\\`root\\'" "/ssh:%h:"))

;; tramp need perl or other encode or decode tools.
;; Plato Wu,2009/06/04: sudo: is NG in tramp but sudo:: is OK. because sudo::
;; is shortend syntax for root@localhost
;; Plato,08/08/23: it need add NOPASSWD: /bin/bash, into /etc/sudoers.
;; Plato Wu,2010/01/17: If there is pscp(search by executable-find), tramp
;; will use it, so pscp must be at exec-path
;; Plato Wu,2010/04/06: make sure putty use xterm-256color, so tramp can works
;; in putty
;; Plato Wu,2010/05/23: use list-colors-display to confirm if emacs can use 256 colors
(defun xwl-revert-buffer-with-sudo ()
  "Revert buffer using tramp sudo.
    This will also reserve changes already made by a non-root user."
  (interactive)
  (let ((f (buffer-file-name)))
    (when f
      (let ((content (when (buffer-modified-p)
		       (widen)
		       (buffer-string))))
	  ;; Plato, 08/08/06: remarks for permitting swtich into root mode first,
          ;; some root's file can not be read.
;; 	(if (file-writable-p f)
;; 	    (revert-buffer)
	  (kill-buffer (current-buffer))
	  (if (file-remote-p f)
	      (find-file
	       (replace-regexp-in-string "^\\/[^:]+:" "/sudo:" f))
	    (find-file (concat "/sudo::" f)))
	  (when content
	    (let ((buffer-read-only nil))
	      (erase-buffer)
	      (insert content)))))))

(global-set-key (kbd "C-c s") 'xwl-revert-buffer-with-sudo)

;; Plato Wu,2010/09/13: ignore annoy query.
(defun save-buffers-kill-client (&optional arg)
  "Offer to save each buffer, if there is only client,
   kill Emacs itself, otherwise kill the current connection"
  (interactive "P")
  (if (> (length server-clients) 1)
      (server-save-buffers-kill-terminal arg)
    (progn 
      (remove-hook 'kill-emacs-query-functions 'server-kill-emacs-query-function)
      (if (functionp 'quit-slime)
          (quit-slime))
      (if (and (functionp 'gnus-alive-p)
               (gnus-alive-p))
          (gnus-group-exit))
      (save-buffers-kill-emacs arg))))

;; Plato Wu,2008/10/08 "^X^C" is not "" which is inputed by C-q C-x C-q C-c
(unless (or (is-system "cygwin") window-system)
  (define-key global-map "" 'save-buffers-kill-client))

(defun my-done ()
    (interactive) 
    (server-edit)
    (make-frame-invisible nil t))

(unless (or (is-system "cygwin") 
            (is-version 21))
  (if (is-system "windows-nt")
      (global-set-key (kbd "C-z") 'my-done)
    (global-set-key "\C-z" 'delete-frame)))

;; set the following var to t if you like a newline to the end of copied text.
(defvar my-kill-ring-save-include-last-newline nil)
;; set the following var to t if you like a newline in the end of killed text.
(defvar my-kill-region-include-last-newline nil)


;; if no region is selected, C-w to kill line, M-w to copy line
(defun my-kill-ring-save (&optional line)
  "This function is a enhancement of `kill-ring-save', which is normal used
to copy a region.  This function will do exactly as `kill-ring-save' if
there is a region selected when it is called. If there is no region, then do
copy lines as `yy' in vim."
  (interactive "P")
  (unless (or line (and mark-active (not (equal (mark) (point)))))
    (setq line 1))
  ;; Plato Wu,2008/11/18 add support () copy
  ;; Plato Wu,2008/11/27 if mark is active do not use () copy.
  (cond 
   ((and (not mark-active) (looking-at "\\s\("))
    (let ((beg (point))
	  (end (scan-lists (point) 1 0))
	  )
      (kill-ring-save beg end)))
   ;; Plato Wu,2009/03/02: sepecial for copy in *w3m* for
   ;; translation Wuala project.
   ((and (not mark-active) (eq major-mode 'w3m-mode))
    (let ((link (w3m-anchor)))
      (if (not link)
	  (kill-ring-save (point) (line-end-position))
        (message "Copied \"%s\" to kill-ring." link)
	(kill-new link))))
   (line
    (let (
	  ;; (beg (line-beginning-position))
	  ;; 08/06/18, Modify by Plato for using current cursor
	  (beg (point))
	  (end (line-end-position)))
      (when (>= line 2)
	(setq end (line-end-position line)))
      (when (<= line -2)
	(setq beg (line-beginning-position (+ line 2))))
      (if (and my-kill-ring-save-include-last-newline
	       (not (= end (point-max))))
	  (setq end (1+ end)))
      (kill-ring-save beg end)))
   (t (call-interactively 'kill-ring-save))))

(defun my-kill-region (&optional line)
  "This function is a enhancement of `kill-region', which is normal used to
kill a region to kill-ring.  This function will do exactly as `kill-region'
if there is a region selected when it is called. If there is no region, then
do kill lines as `dd' in vim."
  (interactive "P")
  (unless (or line (and mark-active (not (equal (mark) (point)))))
    (setq line 1))
  ;; Plato Wu,2009/01/05: add () kill
  (if (and (not mark-active) (looking-at "\\s\("))
      (let ((beg (point))
	    (end (scan-lists (point) 1 0)))
	(kill-region beg end))
   (if line
       (let ((beg (line-beginning-position))
	     (end (line-end-position)))
	 (when (>= line 2)
	   (setq end (line-end-position line)))
	 (when (<= line -2)
	   (setq beg (line-beginning-position (+ line 2))))
	 (if (and my-kill-region-include-last-newline
		  (not (= end (point-max))))
	     (setq end (1+ end)))
	 (kill-region beg end))
     (call-interactively 'kill-region))))

;; bind it
(global-set-key [?\C-w] 'my-kill-region)

;; bind it
(global-set-key "\M-w" 'my-kill-ring-save)

(defun view-msn-log ()
  (interactive)
  (let* ((filename (file-name-nondirectory (buffer-file-name)))
        (output-buffer (get-buffer-create (concat filename ".html"))))
    (with-current-buffer output-buffer
      (call-process "xsltproc" nil t t filename)
      (browse-url-of-buffer))))

;; Plato Wu,2011/01/23: it is need by blogger-configuration and org-toodledo-configuration
(require 'netrc)


;;;;C-. set mark; C-, goto mark
;;(global-set-key [(control ?\.)] 'ska-point-to-register)
;;(global-set-key [(control ?\,)] 'ska-jump-to-register)
;; f12 set mark; S-F12 goto mark in ssh mode
(global-set-key [f12] 'ska-point-to-register)
;; Plato Wu,2009/12/17: Alt+F12 does not work in X window and Windows
;; Ctrl+F12 does not work in termial, so enable them both.
(global-set-key (quote [C-f12]) 'ska-jump-to-register)
(global-set-key (quote [27 f12]) 'ska-jump-to-register)
;; Plato Wu,2010/04/07: M-f12 for windows
(global-set-key (quote [M f12]) 'ska-jump-to-register)



(defun ska-point-to-register()
  "Store cursorposition _fast_ in a register. 
Use ska-jump-to-register to jump back to the stored 
position."
  (interactive)
  (point-to-register 8))

(defun ska-jump-to-register()
  "Switches between current cursorposition and position
that was stored with ska-point-to-register."
  (interactive)
  (let ((tmp (point-marker)))
        (jump-to-register 8)
        (set-register 8 tmp)))

;; Plato Wu,2009/12/25: emacs 23 has rgrep now
(unless (is-version 23)
                                        ;grep-find's pro version which filters files which does not blong current
                                        ;file's class
  (defvar wcy-find-grep-file-class
    '(
      ;; Plato Wu,2009/12/08: Dos prompt use | as key word so it can not be used at regrex expression
      ;; ( ".*\\.h$"
      ;;       ".*\\.c$"
      ;;       ".*\\.cc$"
      ;;       ".*\\.cxx$"
      ;;       ".*\\.cpp$")
      (".*\\.[ch]")
      (".*\\.el$")
      ;; Plato Wu,2008/11/24, Add other formats
      (".*\\.lisp$")
      (".*\\.htm$" ".*\\.html$")))

  (defvar wcy-find-grep-file-class-history nil)

  (if (boundp 'tool-bar-map)
      (tool-bar-add-item "search-in-file"
                         'wcy-find-grep
                         'wcy-find-grep
                         :help "search in files"))
  ;; Plato Wu,2009/10/19: use find -print0 and xargs -0 to handle files whose
  ;; name contain space.
  (defun wcy-find-grep-internal (regexp dir-name regexp-for-filter-file)
    ;; 08/7/30: modify by Plato for grep -e has a problem in eshell.
    (let ((my-grep-command (concat 
                            ;; 			  " grep  -e '"
                            "'"
                            (mapconcat 'identity regexp-for-filter-file "|")
                            ;; 			  "'" 
                            "' -print0 "
                            )))
      (setq dir-name (expand-file-name dir-name))
      (grep-find (concat "find "
                         dir-name
                         ;;                        "  -type f -print | "
                         ;; Plato Wu,2009/11/18: use iregex let the match being case insensitive
                         " -type f -iregex "
                         my-grep-command
                         ;; Plato Wu,2008/12/07: About xargs options, -E is POSIX
                         ;; compliant but -e is not, they both specify eof-str,
                         ;; so if need use xargs in Mac OS X, use -E; Besides -e
                         ;; is used without eof-str equals neither -E or -e is
                         ;; used.
                         " | xargs -0 grep -H -n -e '"		       
                         regexp
                         "'")))
    (with-current-buffer compilation-last-buffer
      (local-set-key (kbd "C-l") 'wcy-find-grep-hide-dirname)
      (setq default-directory dir-name)
      (wcy-find-grep-hide-dirname)))

  (defun wcy-find-grep-hide-dirname ()
    (interactive)
    (if (not 
         ;; Modify by Plato for it seems it may be grep-mode not compilation-mode
         ;; (eq major-mode 'compilation-mode)
         ;; Plato Wu,2009/09/22: in Cygwin, it is compilation-mode
         (or (eq major-mode 'grep-mode) (eq major-mode 'compilation-mode)))
        (error "must run this command under grep-find mode buffer")
      (save-excursion
        (goto-char (point-min))
        (let* ((dir-name default-directory)
               (r (re-search-forward (concat "^" (regexp-quote dir-name)) nil t))
               begin end)              ;progn;remove progn for warning
          (while r
            (setq begin (match-beginning 0))
            (setq end (match-end 0))
            (let* ((x (overlays-in begin end))
                   (e (or (and x (car x)) (make-overlay begin end))))
              (overlay-put e 'invisible (not (overlay-get e 'invisible))))
            (setq r (re-search-forward (concat "^" (regexp-quote dir-name)) nil t )))))))

  ;; Plato Wu,2009/12/20: use rgrep command instead? TO check
  (defun wcy-find-grep (regexp dir-name regexp-for-fileter-file)
    (interactive (list  
                  (read-from-minibuffer 
                   "Regexp:"
                   ; Modify by Plato for It seems <> is invalid.
                   ; (concat "\\<"
                   (regexp-quote
                   ; Plato,08/08/27: current-word might return nil
                    (or (current-word nil) ""))
                   ; "\\>");; initial content is nil
                   nil                  ;; key map is nil
                   nil ;; read as lisp expression, false;
                   'regexp-history
                   nil                            ;; not default
                   nil ;;inherit-input-method
                   )
                  (read-file-name (format "Directory(recursively):" )
                                  nil default-directory nil)
                  (if (buffer-file-name (current-buffer))
                      nil
                    (read-from-minibuffer "File Regexp:"
                                          nil
                                          nil
                                          nil
                                          'wcy-find-grep-file-class-history
                                          nil
                                          nil))))
    (let ((files-filter
           (if (null regexp-for-fileter-file)
               (dolist-if (var wcy-find-grep-file-class)
                          ;; Plato Wu,2009/11/18: let it ignores case
                          (let ((case-fold-search t))
                            (string-match
                             (mapconcat 'identity var "\\|")
                             (buffer-file-name (current-buffer))))
                          var)
             (list (list regexp-for-fileter-file)))))
      (wcy-find-grep-internal regexp  dir-name (apply 'append files-filter))))
  (defmacro dolist-if ( pair condtion body)
    (list 'let '(result)
          (list 'dolist  (append pair '(result))
                (list 'if condtion
                      (list 'setq 'result
                            (list 'append 'result
                                  (list 'list
                                        (list 'progn
                                              body)))))))))

(defun cg-erc ()
  (interactive)
    ;; Plato Wu,2010/04/01: use 7000 in China Telecom network and
    ;; 6667 in Greatwall network
  (let ((netrc-data (netrc-machine (netrc-parse my-authinfo) "irc.freenode.net" "6667")))
    (require 'erc)
    (setq erc-autojoin-channels-alist '(("freenode.net" "#stumpwm")))
    (setq erc-modules (cons 'log erc-modules))
    (erc :server "irc.freenode.net" 
           :port 6667 
           :nick (cdr (assoc "login" netrc-data)) 
           :password (cdr (assoc "password" netrc-data)) 
           :full-name "Plato Wu")))

(when (and (is-system "cygwin") (is-version 21))
  (require 'midnight)
  (defun clean-buffer-list ()
    "Kill old buffers that have not been displayed recently.
The relevant variables are `clean-buffer-list-delay-general',
`clean-buffer-list-delay-special', `clean-buffer-list-kill-buffer-names',
`clean-buffer-list-kill-never-buffer-names',
`clean-buffer-list-kill-regexps' and
`clean-buffer-list-kill-never-regexps'.
While processing buffers, this procedure displays messages containing
the current date/time, buffer name, how many seconds ago it was
displayed (can be nil if the buffer was never displayed) and its
lifetime, i.e., its \"age\" when it will be purged."
    (interactive)
    (let ((tm (float-time)) bts (ts (format-time-string "%Y-%m-%d %T"))
          delay cbld bn)
      (dolist (buf (buffer-list))
        (when (buffer-live-p buf)
          (setq bts (midnight-buffer-display-time buf) bn (buffer-name buf)
                delay (if bts (- tm bts) 0) cbld (clean-buffer-list-delay bn))
          ;; Plato Wu,2009/09/25: use fround intead of round in emacs 21 of Cygwin
          (message "[%s] `%s' [%s %d]" ts bn (if bts (fround delay)) cbld)
          (unless (or (midnight-find bn clean-buffer-list-kill-never-regexps
                                     'string-match)
                      (midnight-find bn clean-buffer-list-kill-never-buffer-names
                                     'string-equal)
                      (get-buffer-process buf)
                      (and (buffer-file-name buf) (buffer-modified-p buf))
                      (get-buffer-window buf 'visible) (< delay cbld))
            (message "[%s] killing `%s'" ts bn)
            (kill-buffer buf)))))))


(provide 'my-utility)