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
  ;; ;; Plato Wu,2009/11/12: emacs 21.2.1 in cygwin does not have this code passage.
  ;; (unless comment-start
  ;;     (let ((cs (read-string "No comment syntax is defined.  Use: ")))
  ;;       (if (zerop (length cs))
  ;;           (error "No comment syntax defined")
  ;;         (set (make-local-variable 'comment-start) cs))))
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
  (if (boundp 'server-clients)
      (if (> (length server-clients) 1)
          (server-save-buffers-kill-terminal arg)
        (progn 
          (remove-hook 'kill-emacs-query-functions 'server-kill-emacs-query-function)
          (if (functionp 'quit-slime)
              (quit-slime))
          (if (and (functionp 'gnus-alive-p)
                   (gnus-alive-p))
              (gnus-group-exit))
          (save-buffers-kill-emacs arg)))
    (save-buffers-kill-emacs arg)))

;; Plato Wu,2008/10/08 "^X^C" is not "" which is inputed by C-q C-x C-q C-c
(unless (or (is-system "cygwin") window-system)
  (define-key global-map "" 'save-buffers-kill-client))



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
  (let ((filename (file-name-nondirectory (buffer-file-name)))
        ;; Plato Wu,2011/05/08: there maybe files which has the same name,
        ;; use buffer-name instead
        (output-buffer (get-buffer-create (concat (buffer-name) ".html"))))
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
;;; use (clean-buffer-list) to clean old buffer which is not visited by 3 day.
;;; it need run after desktop-read
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

(defun comment-dwim-line (&optional arg)
  "Replacement for the comment-dwim command.
If no region is selected and current line is not blank and we are not at the end of the line,
then comment current line.
Replaces default behaviour of comment-dwim, when it inserts comment at the end of the line."
  (interactive "*P")
  (comment-normalize-vars)
  (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    (comment-dwim arg)))

(global-set-key "\M-;" 'comment-dwim-line)

(defun split-window-vertically-and-switch ()
      "split window vertically ands switch to it"
    (interactive)
    (split-window-vertically)
    (other-window 1))

(global-set-key "2" 'split-window-vertically-and-switch)

(defun cg-erc ()
    (interactive)
    ;; Plato Wu,2010/04/01: use 7000 in China Telecom network and
    ;; 6667 in Greatwall network
    (let ((netrc-data (netrc-machine (netrc-parse my-authinfo) "irc.freenode.net" "6667")))
      (erc :server "irc.freenode.net" 
           :port 6667 
           :nick (cdr (assoc "login" netrc-data)) 
           :password (cdr (assoc "password" netrc-data)) 
           :full-name "Plato Wu"))
    (setq erc-modules 
      (append erc-modules
              '(autojoin button completion fill irccontrols list log match menu 
                         move-to-prompt netsplit networks noncommands readonly
                         ring stamp track)))
    ;;(setq erc-autojoin-channels-alist '(("freenode.net" "#symbolicweb")))
    (setq erc-autojoin-channels-alist '(("freenode.net" "#stumpwm"))))


(require 'etags)
;; Plato Wu,2009/04/07: In order to support lookup elisp function.
(defun find-tag-also-for-elisp (tagname &optional next-p regexp-p)
  (interactive 
   ;; Plato Wu,2011/10/19: find-tag-interactive call find-tag-tag call tags-lazy-completion-table
   ;; which require a valid tags table.
   ;; (find-tag-interactive "Find tag: ")
   (list (completing-read "Find tags:"
                     nil
                     nil nil (funcall 'find-tag-default)))) 
   (let ((tagsymbol (intern tagname)))
    (cond
     ((fboundp tagsymbol) 
      (setq find-tag-history (cons tagsymbol find-tag-history))
      (ring-insert find-tag-marker-ring (point-marker))
      (find-function tagsymbol))
     ((boundp tagsymbol) 
      (setq find-tag-history (cons tagsymbol find-tag-history))
      (ring-insert find-tag-marker-ring (point-marker))
      (find-variable tagsymbol))
     (t (find-tag tagname next-p regexp-p)))))

(define-key emacs-lisp-mode-map "\M-." 'find-tag-also-for-elisp)

(unless (or (is-system "cygwin") 
            (is-version 21))
  (defun my-done ()
    (interactive) 
    (server-edit)
    (make-frame-invisible nil t))
  (cond 
   ((is-system "windows-nt")
    (global-set-key (kbd "C-z") 'my-done))
   (t (global-set-key "\C-z" 'delete-frame))))

;; Plato Wu,2010/09/30: revert-buffer will throw a error when the buffer is not modifed
;; it cause revert-buffer-with-coding-system also works weirdly.

(defvar ediff-do-hexl-diff nil
  "variable used to store trigger for doing diff in hexl-mode")
(defadvice ediff-files-internal (around ediff-files-internal-for-binary-files activate)
  "catch the condition when the binary files differ

the reason for catching the error out here (when re-thrown from the inner advice)
is to let the stack continue to unwind before we start the new diff
otherwise some code in the middle of the stack expects some output that
isn't there and triggers an error"
  (let ((file-A (ad-get-arg 0))
        (file-B (ad-get-arg 1))
        ediff-do-hexl-diff)
    (condition-case err
        (progn
          ad-do-it)
      (error
       (if ediff-do-hexl-diff 
           (let ((buf-A (find-file-noselect file-A))
                 (buf-B (find-file-noselect file-B)))
             (with-current-buffer buf-A
               (hexl-mode 1))
             (with-current-buffer buf-B
               (hexl-mode 1))
             (ediff-buffers buf-A buf-B))
         (error (error-message-string err)))))))

(defadvice ediff-setup-diff-regions (around ediff-setup-diff-regions-for-binary-files activate)
  "when binary files differ, set the variable "
  (condition-case err
      (progn
        ad-do-it)
    (error
     (setq ediff-do-hexl-diff
           (and (string-match-p "^Errors in diff output.  Diff output is in.*"
                                (error-message-string err))
                (string-match-p "^\\(Binary \\)?[fF]iles .* and .* differ"
                                (buffer-substring-no-properties
                                 (line-beginning-position)
                                 (line-end-position)))
                (y-or-n-p "The binary files differ, look at the differences in hexl-mode? ")))
     (error (error-message-string err)))))

;; Plato Wu,2011/04/09: use smart-tab instead.
;; (defun my-indent-or-complete ()
;;   ;;   "If cursor is at the end of word then hippie-expand, else indent"
;;    "If cursor is at the end of word then call M-TAB's function, else call
;;    TAB's function."
;;   (interactive)
;;   ;; C-TAB has been binded to tab key's function in mode-hook.
;;   (let ((TAB-func (key-binding '[C-tab]))
;; 	(M-TAB-func (key-binding "\M-\t")))
;;    (if (looking-at "\\>") 
;;        ;;       (hippie-expand nil)
;;        (call-interactively M-TAB-func)
;;      ;;       (indent-for-tab-command)
;;      (call-interactively TAB-func))))

;; some useful fuction
;; (make-variable-buffer-local 'M-TAB-func)
;; (kill-local-variable 'buffer-file-coding-system)
;; (buffer-local-variables)

;; (defun define-my-indent () 
;;   ;; Plato Wu,2008/11/28 latex-mode and LaTeX-mode-map is inconsistent
;;   (let (;; Plato Wu,2008/11/19, it seems in X window & eshell mode [tab] is not "\t"
;; 	;; is (quote [tab]) but this does not work for other modes.
;; 	;(tab-key (quote [tab]))
;; 	(tab-key "\t")
;; 	)
;;     (when (current-local-map)
;;       (let ((TAB-func (key-binding tab-key)))
;; 	(unless (or (eq (key-binding tab-key) 'my-indent-or-complete)
;;                     ;; Plato Wu,2010/01/26: tab in shell and eshell don't have completion-at-point
;;                     ;; function.
;; 		    (string-match "^eshell-mode\\|^shell-mode" (symbol-name major-mode)))
;; 	  ;; Plato Wu,2008/09/24
;; 	  ;; if use string to represent key sequence, it will meet some problem
;; 	  ;; like ^ have been defined in gnus-*-mode, so "^C\t" is NG. Besides,
;; 	  ;; it seems \ can not occur twice in string, so "\C\t" is NG.
;; 	  (define-key (current-local-map) '[C-tab] TAB-func)
;; 	  (define-key (current-local-map) tab-key 'my-indent-or-complete))))))

;; (add-hook 'after-change-major-mode-hook 'define-my-indent)

;; emacs 22 has this function
(if (is-version 21)
 (defadvice comment-or-uncomment-region (before slickcomment activate compile)
   "When called interactively with no active region, toggle comment on current line instead."
   (interactive
    (if mark-active (list (region-beginning) (region-end))
      (list (line-beginning-position)
            (line-beginning-position 2)))))) 

;; Plato Wu,2010/03/08: use F9 instead of C-x `
(defun next-error-cycle ()
  "cycle next error"
  (interactive)
    (condition-case nil
      (next-error)
    (error 
     (message "Reach the end!!!")
     (next-error 1 t))))

(global-set-key [f9] 'next-error-cycle)

(require 'grep)

(defun project-grep (regexp &optional files)
  "grep files in project directory recursively no matter the sub directory the visited
   stay. get direcotry from tag-file-name"
  (interactive
   (let ((regexp (grep-read-regexp)))
     (grep-compute-defaults)
     (list regexp (grep-read-files regexp))))
  (if tags-file-name
      (rgrep regexp files (file-name-directory tags-file-name))
    (rgrep regexp files ".")))

(defun stumpwm ()
  (interactive)
  (slime-connect "localhost" 4005))

(require 'icalendar)
;; Plato Wu,2010/10/02: I don't know why \r can not be found by re-search-forward
;; in a file which has 0x0d. Another problem is there is not \t in p1i's task file
(defadvice icalendar--get-unfolded-buffer (after remove-LF-symbole activate)
  (save-current-buffer
      (set-buffer ad-return-value)
      (goto-char (point-min))
      (while (re-search-forward "=\r?\n" nil t)
        (replace-match "" nil nil)))
  ad-return-value)

(setq icalendar-debug t)

(defun icalenar-convert-date (e symbol zone-map)
  (let* ((symbol-value (icalendar--get-event-property e symbol))
       (symbol-zone (icalendar--find-time-zone
                  (icalendar--get-event-property-attributes
                   e symbol)
                  zone-map))
       (symbol-dec (icalendar--decode-isodatetime symbol-value nil
                                               symbol-zone)))
    (if symbol-dec
;         (icalendar--datetime-to-diary-date symbol-dec)
        ;(icalendar--datetime-to-colontime symbol-dec)
        (apply 'encode-time symbol-dec)
      nil)))

(defun org-import-p1i-task ()
  (interactive)
  (mapcar 
  '(lambda (ics-filename)
     (org-import-p1i-task-internal ics-filename "~/tmp.org"))
  (file-expand-wildcards "/opt/Funambol/ds-server/db/vtask/plato.wu/*"))
  (set-file-times "~/.import_p1i_task_done"))

(defun org-import-p1i-task-internal (ical-filename org-filename) 
  (progn ;save-current-buffer
    (set-buffer (find-file ical-filename))
    ;; prepare ical
    (message "Preparing icalendar...")
    (set-buffer (icalendar--get-unfolded-buffer (current-buffer)))
    (goto-char (point-min))
    (message "Preparing icalendar...done")
    (if (re-search-forward "^BEGIN:VCALENDAR\\s-*$" nil t)
        (let (ical-contents)
    
          ;; read ical
          (message "Reading icalendar...")
          (beginning-of-line)
          (setq ical-contents (icalendar--read-element nil nil))
          (message "Reading icalendar...done")
          ;; convert ical
          (message "Converting icalendar...")
          (dolist (ical-content ical-contents)
            (let* ((ev (icalendar--get-children ical-content 'VTODO))
                 (zone-map (icalendar--convert-all-timezones ical-contents))
                 (error-string "")
                 (found-error t)
                    e)
            ;; Plato Wu,2010/10/05: only one VTODO in one VCALENDAR
            (if ev
              (progn
               (setq e (car ev))
               (setq ev (cdr ev))
               (condition-case error-val
                   (let* ((start-d (icalenar-convert-date e 'DTSTART zone-map))
                          (due-d (icalenar-convert-date e 'DUE zone-map))
                          (complete-d (icalenar-convert-date e 'COMPLETED zone-map))
                          (summary 
                           (w3m-url-decode-string 
                            (replace-regexp-in-string 
                             "=" "%" 
                             (icalendar--convert-string-for-import
                              (or (icalendar--get-event-property e 'SUMMARY)
                                  "No summary")))))
                          (description 
                           (w3m-url-decode-string 
                            (replace-regexp-in-string 
                             "=" "%" 
                             (icalendar--convert-string-for-import
                              (or (icalendar--get-event-property e 'DESCRIPTION)
                                  "No description"))))))
                     (icalendar--dmsg "complete-d %s: `%s'" complete-d summary)
                     ;; check whether start-time is missing
                     (icalendar--dmsg "start-d: %s, due-d: %s" start-d due-d)
                     ;; add all other elements unless the user doesn't want to have
                     ;; them
                     (icalendar--dmsg "dscription is %s" description)
                     ;; Plato Wu,2010/10/05: only backup completed task which date is
                     ;; later than ~/.import_p1i_task_done
                     (when t ;; (and complete-d 
                           ;;      (time-less-p 
                           ;;       (fifth (file-attributes  "~/.import_p1i_task_done"))
                           ;;       complete-d))
                       (set-buffer (find-file org-filename))
                       (goto-char (point-min))
                       (org-end-of-subtree) ;; Plato Wu,2012/04/07: it must contain an outline
                       (insert "\n** TODO " summary "\n" description)
                       (fill-paragraph)
                       (if start-d (org-schedule nil start-d))
                       (if due-d (org-deadline nil due-d))
                       (let ((org-log-done nil)) 
                         (org-todo))
                       (org-add-planning-info 'closed complete-d)
                       ;(org-toodledo-sync-task)
                       ))
                
                 ;; FIXME: inform user about ignored event properties
                 ;; handle errors
                 (error
                  (message "Ignoring event \"%s\"" e)
                  (setq found-error nil)
                  (setq error-string (format "%s\n%s\nCannot handle this event: %s"
                                             error-val error-string e))
                  (message "%s" error-string)))))
          ;; return t if no error occurred            
            (not found-error))))
      ;; (message
      ;;  "Current buffer does not contain icalendar contents!")
      ;; ;; return nil, i.e. import did not work
      ;; nil
      )
    ;(kill-buffer (find-file ical-filename))
    ))

(defun ielm2 nil
  "Interactively evaluate Emacs Lisp expressions.
Switches to the buffer `*ielm*' in other window, or creates it if it does not exist."
  (interactive)
  (load "ielm")
  (let (old-point)
    (unless (comint-check-proc "*ielm*")
      (with-current-buffer (get-buffer-create "*ielm*")
	(unless (zerop (buffer-size)) (setq old-point (point)))
	(inferior-emacs-lisp-mode)))
    (pop-to-buffer "*ielm*" t)
    (when old-point (push-mark old-point))))

(defun hex-to-ascii (b e)
  "Translate the region from hex to ascii and copy it to clipboard."
  (interactive "r")
  (save-excursion
    (let ((i e)
           (x-select-enable-clipboard t)
           s)
      (while (> i b)
        (setq s (concat (format "%c" (read (concat "#x" (buffer-substring-no-properties (- i 2) i)))) s))
        ;; Plato Wu,2012/07/26: using 3 to skip space.
        (setq i (- i 3)))
      (kill-new s t)
      (message (format "%s copied to clip board" s)))))

(defun ascii-to-hex (b e)
  "Translate an ascii string to a hex string and copy it to clipboard"
  (interactive "r")
  (save-excursion
    (let ((i b)
          (x-select-enable-clipboard t)
          s)
      (while (< i e)
        (setq s (concat s (format "%x " (get-byte i))))
        (setq i (+ i 1)))
      (kill-new s t)
      (message s))))

;; source: http://steve.yegge.googlepages.com/my-dot-emacs-file
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file name new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

(provide 'my-utility)
