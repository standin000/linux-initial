(defun eshell-configuration ()
  ;emacs21 uses eshell-ask-to-save-history but emacs 22 & 23 uses
  ;eshell-save-history-on-exit
  (if (is-version 21)
    (setq eshell-ask-to-save-history 'always)
    (setq eshell-save-history-on-exit t))
  (setq eshell-prompt-function 
      (function
       (lambda ()
	 (let ((pwd (eshell/pwd)))
	   (if (< (length pwd) 30)
	       (concat pwd
		       (if (= (user-uid) 0) " # " " $ "))
	     (concat (concat "..." (subseq pwd 20)) 
		     (if (= (user-uid) 0) " # " " $ ")))))))
  ;remove duplicate history items.
  (setq eshell-hist-ignoredups t)
  ;; ;; Plato Wu,2009/04/10: it seems there is not effect in emacs 22.2.1 To check
  ;; ;; let eshell show color but it is slow.
  ;; (require 'ansi-color)
  ;; (add-hook 'eshell-preoutput-filter-functions
  ;;           'ansi-color-apply)
 ; (autoload 'ansi-color-apply "ansi-color" nil t)
  ;; Do not let eshell show color
  ;; (add-hook 'eshell-preoutput-filter-functions 'ansi-color-filter-apply)

  ;; Dealing With Wildcards and Multiple Files in eshell
 ;; Assume you want to define an alias ‘emacs’ for ‘find-file’. See EshellAlias for a simple alias solution. The problem is that you cannot open multiple files that way. Using wildcards would also be a problem if these expand into multiple filenames. Instead of using an alias, use the following function.
  (defun eshell/emacs (&rest args)
    "Open a file in emacs. Some habits die hard."
    (if (null args)
	;; If I just ran "emacs", I probably expect to be launching
	;; Emacs, which is rather silly since I'm already in Emacs.
	;; So just pretend to do what I ask.
	(bury-buffer)
      ;; We have to expand the file names or else naming a directory in an
      ;; argument causes later arguments to be looked for in that directory,
      ;; not the starting directory
      (mapc #'find-file 
	    (mapcar #'expand-file-name (eshell-flatten-list (reverse args))))))

  (defun eshell-maybe-bol ()
    (interactive)
    (let ((p (point)))
      (eshell-bol)
      (if (= p (point))
	  (beginning-of-line))))

  (add-hook 'eshell-mode-hook
	    '(lambda () 
	       (define-key eshell-mode-map "\C-a" 'eshell-maybe-bol)))

  (defun eshel ()
    "change to visited file's directory every time"
    (interactive)
    (let ((visited-file-directory default-directory))
      (eshell)
      (eshell/cd visited-file-directory)
      ;there may be dangerous command input!
      (if (string= (eshell-get-old-input) "")
	  (eshell-send-input)
	  (insert-and-inherit "dangeours!"))))
  ;; Plato Wu,2010/07/06: It seems eshell in Windoes will use alias in .bash_profile first.
  (setq eshell-command-aliases-list 
      '(("rm" "~/linux-backup/movetotrash.sh $*")
        ;; Plato Wu,2010/06/23: /bin don't work for Emacs in Windows
	("del" "/bin/rm -i -f $*")
;	("cp" "/bin/cp -i $*")
        ;; Plato Wu,2010/09/13: rm -i cp -i don't work for Emacs in Linux
;	
	("cp" "/bin/cp -i $*")
	("df" "df -h $* ")
	("du" "du -h $*")
	("less" "less -r $*")
	("whence" "type -a $*")
	("grep" "grep --color $*")
	("dir" "ls --format vertical $*")
	("vdir" "ls --format=long $*")
	("ll" "ls -l $*")
	("la" "ls -A $*")
	("l" "ls -CF $*")
;	("ls" "ls -hF $*")
	("ls" "ls -hF --color=auto $*")
	("mv" "mv -i $1 $2")
	("free" "free -m")
	("vi" "emacs $*")
	("nano" "emacs $*")))
  (when (is-system "windows-nt")
      (setcdr (assoc "ls" eshell-command-aliases-list) '("ls -h $*"))
      ;; Plato Wu,2010/11/03: I don't know why $* does work with $*
      (setcdr (assoc "cp" eshell-command-aliases-list) '("cp $1 $2"))
      ;; Plato Wu,2010/11/03: Don't active rm command for it could make use of movetotrash.sh
;;      (setcdr (assoc "rm" eshell-command-aliases-list) '("rm -i -f"))
      (setcdr (assoc "del" eshell-command-aliases-list) '("rm -i -f $*"))))

(eshell-configuration)

(defun paredit-configuration ()
  ;; Plato Wu,2008/09/27, paredit will cause alt-s, ctrl-d can not used in ido mode
;;  (add-hook 'minibuffer-setup-hook '(lambda () (paredit-mode +1)))
;; Plato, 08/09/04, fundamental mode is a base mode, so it doesn't have
;; a mode hook, it only run change-major-mode-hook but enter others mode
;; should enter fundamental mode first, so this hook would be run always.
  (mapc (lambda (mode)
	  (let ((hook (intern (concat (symbol-name mode)
				      "-mode-hook"))))
	    (add-hook hook (lambda () (paredit-mode +1)))))
	'(emacs-lisp lisp slime-repl scheme ielm))
  (defadvice ielm-eval-input (after ielm-paredit activate)
    "Begin each IELM prompt with a ParEdit parenthesis pair."
    (paredit-open-round))
  (add-hook 'ielm-mode-hook
            '(lambda () 
               (setq comint-input-ring-file-name "~/.ielm.history")
               (turn-on-eldoc-mode)))
  ;; Plato Wu,2009/12/09: enable eldoc mode.
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode))

(defun ido-configuration ()
  (setq ido-enable-tramp-completion nil) 
;; Plato Wu,2009/06/04: If it is mess, try to use ido-wash-history 
  (setq ido-ignore-buffers
	'("^ .*"
	;; ignore *eshell*, *svn-status*, a awkward regular expression
	;; for I do not know how to perfect match a word which is not "eshell"
	;; or "svn-status".
	  "^\\*[^es].\\{3\\}[^s].*"
	  "TAGS"))
  (setq ido-record-ftp-work-directories nil)

;; Plato Wu,2009/06/04: let ido-work-directory-list not record /sudo
;; so that ido do not need wait 60s for visit /sudo
  (setq ido-work-directory-list-ignore-regexps '("/sudo:"))
  ;; Plato Wu,2008/12/09: remarks for key is conflict with redshank mode
  (define-key ctl-x-map "\C-r" nil)

  (ido-mode t))

(defun psvn-configuration ()
  ;; Plato Wu,2009/03/31: This code is used in PMP project in Kinpo
;; to handle svn user name which includes blanks
;; (setq svn-user-names-including-blanks
;;      '("elan zhou" "mike ma" "plato wu" "howardkoo gu"))
;; (add-hook 'svn-pre-parse-status-hook
;; 	  'svn-status-parse-fixup-user-names-including-blanks)
  (setq svn-log-edit-show-diff-for-commit t
        svn-status-hide-unmodified t
        ;; Plato Wu,2009/07/27: disable modeline mark display for there
        ;; is not image for console emacs.
        svn-status-state-mark-modeline nil))

(defun dired-single-configuration ()
;  (require 'dired)
  (define-key dired-mode-map (kbd "RET") 'joc-dired-single-buffer)
  (define-key dired-mode-map 
    (kbd ".")
    '(lambda ()
       (interactive)
       (joc-dired-single-buffer ".."))))

(defun wdired-configuration ()  
  (require 'wdired)
  (setq wdired-allow-to-change-permissions t)
  (define-key dired-mode-map "r"
    'wdired-change-to-wdired-mode))

(wdired-configuration)

;; use h show help
(require 'ibuffer)
;; Plato Wu,2011/05/08: ibuffer is a part of Emacs 22 and there is no 
;; ibuffer-and-update now and ibuffer update automatically
(global-set-key (kbd "C-x C-b") 'ibuffer)

(defun emms-configuration ()
  (require 'emms-setup)
  (require 'emms-player-mpd)
  (require 'emms-history)
  (require 'emms-score)
  (require 'emms-i18n)
  (require 'emms-volume)
  (emms-standard)
  (emms-default-players)
  (add-to-list 'emms-player-list 'emms-player-mpd)
  (add-to-list 'emms-info-functions 'emms-info-mpd)
  (emms-player-set emms-player-mpd 'regex
                   "\\.ogg\\|\\.mp3\\|\\.wma\\|\\.ogm\\|\\.asf\\|\\.mkv\\|http://\\|mms://\\|\\.rmvb\\|\\.flac\\|\\.vob\\|\\.m4a\\|\\.ape\\|\\.mpc")
  (emms-player-set emms-player-mplayer 'regex
                   "\\.wav\\|\\.pls+\\|\\.mpg\\|\\.mpeg\\|\\.wmv\\|\\.wma\\|\\.mov\\|\\.avi\\|\\.divx\\|\\.ogm\\|\\.asf\\|\\.mkv\\|.rm\\|\\.rmvb\\|\\.mp4|\\.rm")

  ;; Show the current track each time EMMS
  ;; starts to play a track with "NP : "
  (add-hook 'emms-player-started-hook 'emms-show)
  (setq emms-volume-change-function 'emms-volume-mpd-change)
  (setq emms-show-format "%s")
  (setq emms-player-next-function 'emms-random)
  (setq emms-playlist-buffer-name "*Music*")
  (setq emms-player-mpd-music-directory "~/Music")
  (setq emms-player-next-function 'emms-next-noerror)
  (emms-score 1)
  (defun my-stop-player ()
    "Stop emms player."
    (interactive)
;    (shell-command "mpd --kill &")
    (emms-playlist-current-kill)
    (emms-player-mpd-disconnect))
  (defun my-start-player ()
    "Start MPD and sync to its playlistemms player."
    (interactive)
;    (shell-command "mpd &") ; uses default ~/.mpdconf
;    (shell-command "sleep 3 ") 
    (emms-player-mpd-connect)
    (switch-to-buffer emms-playlist-buffer))
  ;; run (emms-history-save) first
  ;; emms-shuffle will shuffle the playlist for mpd don't support random play
  (emms-history-load)
  ) 

(defvar my-authinfo "~/.authinfo")


(defun blogger-configuration ()
  (require 'muse) 
  (require 'muse-mode) 
  (require 'muse-publish) 
  (require 'muse-html)
  (require 'weblogger)
  (setq muse-insert-url-initial-input nil)
  (add-hook 'muse-mode-hook 
            '(lambda ()
               (flyspell-mode)
               (footnote-mode)          ;C-c ! a Footnote-add-footnote
               (setq case-fold-search t)
               (unless (re-search-forward "^#title" nil t)
                 (insert (format "#title %s\n" (replace-regexp-in-string ".muse" "" (buffer-name)))))))
  ;; Plato Wu,2010/01/25: support .JPG as image, case-fold-search
  ;; is always bound to nil
  (setq muse-image-regexp 
        (concat "\\.JPG\\|"  muse-image-regexp))

  (defvar muse-blogger-markup-strings nil
  
    "Strings used for marking up text as XHMTL for Blogger.")

  (setq muse-blogger-markup-strings
        '((section . "<h4>")
          (section-end . "</h4>")
          (subsection . "<h5>")
          (subsection-end . "</h5>")
          (subsubsection . "<h6>")
          (subsubsection-end . "</h6>")
          (section-other . "<div>")
          (section-other-end . "</div>")
          (begin-example . "<pre class='src'>")
          (rule . "--------华-------丽-------的-------分-------割-------线-------")
          (image . "<img src=\"%s.%s\" width=85%% height=85%% alt=\"\">")))

  (muse-derive-style
   "blogger" "xhtml1.1"
   :header  "Subject: <lisp>(muse-publishing-directive \"title\")</lisp>
From: <lisp>(muse-publishing-directive \"author\")</lisp>
Keywords: <lisp>(muse-publishing-directive \"categories\")</lisp>
Summary: <lisp>(muse-publishing-directive \"labels\")</lisp>
Message-ID: <lisp>(muse-publishing-directive \"message-id\")</lisp>
Newsgroup: <lisp>(muse-publishing-directive \"newsgroup\")</lisp>
Date: <lisp>(muse-publishing-directive \"date\")</lisp>
--text follows this line--
"
   :footer  ""
   :regexps nil
   :strings 'muse-blogger-markup-strings)
  ;; Plato Wu,2010/01/10: Set ISO 8601 time format.
  ;; time zone is indispensable for the blog server use different time zone
  ;; Plato Wu,2010/01/24: It is useless for set time zone, the server will always
  ;; use UTC time for output.
  (setq muse-publish-date-format "%Y%m%dT%H:%M:%S%z");

  ;;(setq muse-html-markup-paragraph nil)

  (defun blogger-login ()
                                        ;(interactive)
    ;; (unless weblogger-entry-ring
    ;;   (setq weblogger-entry-ring (make-ring weblogger-max-entries-in-ring)))
    ;; (ring-insert weblogger-entry-ring '(("content" "")))
    ;; (setq weblogger-ring-index 0)
    (let ((netrc-data (netrc-machine (netrc-parse my-authinfo) "platowu.info" "80")))
      (setq weblogger-config-alist 
            (list (list "default"  
                        ;; Plato Wu,2010/09/01: use account instead default, default
                        ;; is entry when net-machine will return if there is not match entry
                        (cdr (assoc "account" netrc-data))
                        (cdr (assoc "login" netrc-data))
                        (cdr (assoc "password" netrc-data)) "1")))
      ;; (setq weblogger-config-alist 
      ;;     (list (list "default"  
      ;;            (cons "user" (cdr (assoc "login" netrc-data)))
      ;;            ;; Plato Wu,2010/09/01: use account instead default, default is entry when net-machine
      ;;            ;; will return if there is not match entry
      ;;            (cons "server-url" (cdr (assoc "account" netrc-data)))
      ;;            (cons "pass" (cdr (assoc "password" netrc-data)))
      ;;            (cons "weblog" 1))))
      )
    (weblogger-select-configuration))
                                        ;  (setq-default case-fold-search t)
  (defun blogger-post ()
    (interactive)
    (unless weblogger-config-alist 
      (let ((buffer (current-buffer))) 
          (blogger-login)
          (switch-to-buffer buffer)))
    (save-buffer)
    (goto-char (point-min))
    (let ((struct nil)
          (new (not (re-search-forward "^#message-id" nil t))))
      ;; Plato Wu,2010/01/25: ignore case so that .JPG is recongized as a image.
      (let ((case-fold-search t)) 
        (muse-publish-this-file (muse-style "blogger") "/tmp" t))
      (find-file (expand-file-name (muse-publish-output-name buffer-file-name (muse-style "blogger")) "/tmp"))
      (weblogger-entry-mode)
      (message-goto-body)
                                        ;replace single newline in <p></p> for wordpress 
      (while (re-search-forward "\n\n" nil t)
        (replace-match "\r\r" nil t))
      (message-goto-body)
      (while (re-search-forward "\n" nil t)
        (replace-match "" nil t))
      (message-goto-body)
      (while (re-search-forward "\r\r" nil t)
        (replace-match "\n\n" nil t))
      (setq struct (weblogger-entry-buffer-to-struct))
      (if new
          (weblogger-api-new-entry struct t)
        (weblogger-api-send-edits struct t))
      (set-buffer-modified-p nil)
      (kill-buffer)
      (when new 
        (let ((entry (ring-ref weblogger-entry-ring
                               weblogger-ring-index)))
         (goto-char (point-min))
         (insert (concat "#message-id "  
                         (format "<%s/%s@%s>"
                                 (cdr (assoc "entry-id" entry))
                                 (weblogger-weblog-id)
                                 (url-host (url-generic-parse-url weblogger-server-url)))
                         "\n"))
         (if (not (re-search-forward "^#date" nil t))
             (insert (concat "#date "
                             (format-time-string "%Y-%m-%d %H:%M:%S %z"
                                                 (caddr (assoc "dateCreated" entry)))
                             ;; (cdr (assoc "dateCreated"
                             ;;             (ring-ref
                             ;;              weblogger-entry-ring
                             ;;              weblogger-ring-index)))
                             "\n")))
         (setq user-mail-address "plato.wu@qq.com")
                                        ;      (setq user-mail-address "68697211@qq.com")
         (setq smtpmail-default-smtp-server "smtp.qq.com") 
         (setq smtpmail-smtp-server "smtp.qq.com") 
         (setq send-mail-function 'smtpmail-send-it)
         ;; Plato Wu,2009/12/19: QQ smtp server is garbage!!!
         ;; It can not used in post2qzone for Wordpress, it said content is reject
         ;; It can not used DJ EmailPublish for wordpress
         ;; It need set smtpmail-starttls-credentials as 25 but not 465 in Gnus.
         (setq smtpmail-smtp-service 25
               smtpmail-starttls-credentials 
               '(("smtp.qq.com" 25 nil nil)))
         ;; Plato Wu,2010/09/28: delete weiruan000.platowu@spaces.live.com, Live Space will be shutdown
         ;; use its connect RSS instead.
         (compose-mail "42662703@qzone.qq.com"
                       ;;platowu@qq.com,68697211@qzone.qq.com,
                       (cdr (assoc "title" entry)))
         (let ((content (cdr (assoc "content" entry))))
           ;; (insert (substring content 0 
           ;;                    (+ 4 (string-match "</p>" content))))
           (insert (format "<p>....</p><p>请移步<a href=\"http://platowu.info\">恒永之地</a>查看<a href=\"http://platowu.info/%s\">点我</a>，评论也在那边留哦，:)</p>" 
                           (w3m-url-encode-string 
                            (cdr (assoc "title" entry)) 'utf-8))))
         (mail-send-and-exit)))))

  (defun muse-html-markup-footnote ()
    (cond
     ((get-text-property (match-beginning 0) 'muse-link)
      nil)
     ((= (muse-line-beginning-position) (match-beginning 0))
      (prog1
          (let ((text (match-string 1)))
            (muse-insert-markup
             (concat "<p class=\"footnote\">"
                     "<a class=\"footnum\" name=\"" (muse-publishing-directive "title") "fn." text
                                        ;                   "\" href=\"#fnr." text "\">"
                     "\" href=\"#" (muse-publishing-directive "title") "fnr." text "\">"
                     text ".</a>")))
        (save-excursion
          (save-match-data
            (let* ((beg (goto-char (match-end 0)))
                   (end (and (search-forward "\n\n" nil t)
                             (prog1
                                 (copy-marker (match-beginning 0))
                               (goto-char beg)))))
              (while (re-search-forward (concat "^["
                                                muse-regexp-blank
                                                "]+\\([^\n]\\)")
                                        end t)
                (replace-match "\\1" t)))))
        (replace-match "")))
     (t (let ((text (match-string 1)))
          (muse-insert-markup
           (concat "<sup><a class=\"footref\" name=\"" (muse-publishing-directive "title") "fnr." text
                                        ;                 "\" href=\"#fn." text "\">"
                   "\" href=\"#" (muse-publishing-directive "title") "fn." text "\">"
                   text "</a></sup>")))
        (replace-match ""))))
  (require 'smtpmail)

  (defun smtpmail-send-it ()
    (let ((errbuf (if mail-interactive
                      (generate-new-buffer " smtpmail errors")
                    0))
          (tembuf (generate-new-buffer " smtpmail temp"))
          (case-fold-search nil)
          delimline
          (mailbuf (current-buffer))
          ;; Examine this variable now, so that
          ;; local binding in the mail buffer will take effect.
          (smtpmail-mail-address
           (or (and mail-specify-envelope-from (mail-envelope-from))
               user-mail-address))
          (smtpmail-code-conv-from
           (if enable-multibyte-characters
               (let ((sendmail-coding-system smtpmail-code-conv-from))
                 (select-message-coding-system)))))
      (unwind-protect
          (save-excursion
            (set-buffer tembuf)
            (erase-buffer)
            ;; Use the same `buffer-file-coding-system' as in the mail
            ;; buffer, otherwise any `write-region' invocations (e.g., in
            ;; mail-do-fcc below) will annoy with asking for a suitable
            ;; encoding.
            (set-buffer-file-coding-system smtpmail-code-conv-from nil t)
            (insert-buffer-substring mailbuf)
            (goto-char (point-max))
            ;; require one newline at the end.
            (or (= (preceding-char) ?\n)
                (insert ?\n))
            ;; Change header-delimiter to be what sendmail expects.
            (mail-sendmail-undelimit-header)
            (setq delimline (point-marker))
            ;; (sendmail-synch-aliases)
            (if mail-aliases
                (expand-mail-aliases (point-min) delimline))
            (goto-char (point-min))
            ;; ignore any blank lines in the header
            (while (and (re-search-forward "\n\n\n*" delimline t)
                        (< (point) delimline))
              (replace-match "\n"))
            (let ((case-fold-search t))
              ;; We used to process Resent-... headers here,
              ;; but it was not done properly, and the job
              ;; is done correctly in `smtpmail-deduce-address-list'.
              ;; Don't send out a blank subject line
              (goto-char (point-min))
              (if (re-search-forward "^Subject:\\([ \t]*\n\\)+\\b" delimline t)
                  (replace-match "")
                ;; This one matches a Subject just before the header delimiter.
                (if (and (re-search-forward "^Subject:\\([ \t]*\n\\)+" delimline t)
                         (= (match-end 0) delimline))
                    (replace-match "")))
              ;; Put the "From:" field in unless for some odd reason
              ;; they put one in themselves.
              (goto-char (point-min))
              (if (not (re-search-forward "^From:" delimline t))
                  (let* ((login smtpmail-mail-address)
                         (fullname (user-full-name)))
                    (cond ((eq mail-from-style 'angles)
                           (insert "From: " fullname)
                           (let ((fullname-start (+ (point-min) 6))
                                 (fullname-end (point-marker)))
                             (goto-char fullname-start)
                             ;; Look for a character that cannot appear unquoted
                             ;; according to RFC 822.
                             (if (re-search-forward "[^- !#-'*+/-9=?A-Z^-~]"
                                                    fullname-end 1)
                                 (progn
                                   ;; Quote fullname, escaping specials.
                                   (goto-char fullname-start)
                                   (insert "\"")
                                   (while (re-search-forward "[\"\\]"
                                                             fullname-end 1)
                                     (replace-match "\\\\\\&" t))
                                   (insert "\""))))
                           (insert " <" login ">\n"))
                          ((eq mail-from-style 'parens)
                           (insert "From: " login " (")
                           (let ((fullname-start (point)))
                             (insert fullname)
                             (let ((fullname-end (point-marker)))
                               (goto-char fullname-start)
                               ;; RFC 822 says \ and nonmatching parentheses
                               ;; must be escaped in comments.
                               ;; Escape every instance of ()\ ...
                               (while (re-search-forward "[()\\]" fullname-end 1)
                                 (replace-match "\\\\\\&" t))
                               ;; ... then undo escaping of matching parentheses,
                               ;; including matching nested parentheses.
                               (goto-char fullname-start)
                               (while (re-search-forward
                                       "\\(\\=\\|[^\\]\\(\\\\\\\\\\)*\\)\\\\(\\(\\([^\\]\\|\\\\\\\\\\)*\\)\\\\)"
                                       fullname-end 1)
                                 (replace-match "\\1(\\3)" t)
                                 (goto-char fullname-start))))
                           (insert ")\n"))
                          ((null mail-from-style)
                           (insert "From: " login "\n")))))
              ;; Insert a `Message-Id:' field if there isn't one yet.
              (goto-char (point-min))
              (unless (re-search-forward "^Message-Id:" delimline t)
                (insert "Message-Id: " (message-make-message-id) "\n"))
              ;; Insert a `Date:' field if there isn't one yet.
              (goto-char (point-min))
              (unless (re-search-forward "^Date:" delimline t)
                (insert "Date: " (message-make-date) "\n"))
              ;; Possibly add a MIME header for the current coding system
              (let (charset)
                (goto-char (point-min))
                (and (eq mail-send-nonascii 'mime)
                     (not (re-search-forward "^MIME-version:" delimline t))
                     (progn (skip-chars-forward "\0-\177")
                            (/= (point) (point-max)))
                     smtpmail-code-conv-from
                     (setq charset
                           (coding-system-get smtpmail-code-conv-from
                                              'mime-charset))
                     (goto-char delimline)
                     (insert "MIME-version: 1.0\n"
                             ;; Plato Wu,2009/12/19: use html format
                             "Content-type: text/html; charset="
                             (symbol-name charset)
                             "\nContent-Transfer-Encoding: 8bit\n")))
              ;; Insert an extra newline if we need it to work around
              ;; Sun's bug that swallows newlines.
              (goto-char (1+ delimline))
              (if (eval mail-mailer-swallows-blank-line)
                  (newline))
              ;; Find and handle any FCC fields.
              (goto-char (point-min))
              (if (re-search-forward "^FCC:" delimline t)
                  ;; Force `mail-do-fcc' to use the encoding of the mail
                  ;; buffer to encode outgoing messages on FCC files.
                  (let ((coding-system-for-write smtpmail-code-conv-from))
                    (mail-do-fcc delimline)))
              (if mail-interactive
                  (with-current-buffer errbuf
                    (erase-buffer))))
            ;;
            (setq smtpmail-address-buffer (generate-new-buffer "*smtp-mail*"))
            (setq smtpmail-recipient-address-list
                  (smtpmail-deduce-address-list tembuf (point-min) delimline))
            (kill-buffer smtpmail-address-buffer)

            (smtpmail-do-bcc delimline)
            ;; Send or queue
            (if (not smtpmail-queue-mail)
                (if (not (null smtpmail-recipient-address-list))
                    (if (not (smtpmail-via-smtp
                              smtpmail-recipient-address-list tembuf))
                        (error "Sending failed; SMTP protocol error"))
                  (error "Sending failed; no recipients"))
              (let* ((file-data
                      (expand-file-name
                       (format "%s_%i"
                               (format-time-string "%Y-%m-%d_%H:%M:%S")
                               (setq smtpmail-queue-counter
                                     (1+ smtpmail-queue-counter)))
                       smtpmail-queue-dir))
                     (file-data (convert-standard-filename file-data))
                     (file-elisp (concat file-data ".el"))
                     (buffer-data (create-file-buffer file-data))
                     (buffer-elisp (create-file-buffer file-elisp))
                     (buffer-scratch "*queue-mail*"))
                (unless (file-exists-p smtpmail-queue-dir)
                  (make-directory smtpmail-queue-dir t))
                (with-current-buffer buffer-data
                  (erase-buffer)
                  (set-buffer-file-coding-system smtpmail-code-conv-from nil t)
                  (insert-buffer-substring tembuf)
                  (write-file file-data)
                  (set-buffer buffer-elisp)
                  (erase-buffer)
                  (insert (concat
                           "(setq smtpmail-recipient-address-list '"
                           (prin1-to-string smtpmail-recipient-address-list)
                           ")\n"))
                  (write-file file-elisp)
                  (set-buffer (generate-new-buffer buffer-scratch))
                  (insert (concat file-data "\n"))
                  (append-to-file (point-min)
                                  (point-max)
                                  (expand-file-name smtpmail-queue-index-file
                                                    smtpmail-queue-dir)))
                (kill-buffer buffer-scratch)
                (kill-buffer buffer-data)
                (kill-buffer buffer-elisp))))
        (kill-buffer tembuf)
        (if (bufferp errbuf)
            (kill-buffer errbuf))))))

(defun smart-tab-configuration ()
  (setq smart-tab-using-hippie-expand t)
  (global-smart-tab-mode 1)
  (add-to-list 'smart-tab-disabled-major-modes 'eshell-mode))

(defun org-toodledo-configuration ()
  (require 'org-toodledo)
  (let ((netrc-data 
         (netrc-machine 
          (netrc-parse my-authinfo) 
          "toodledo.com" "80")))
    (setq org-toodledo-userid (cdr (assoc "login" netrc-data)))
    (setq org-toodledo-password (cdr (assoc "password" netrc-data)))
    (setq org-toodledo-archive-location "* Done"))
  ;; Plato Wu,2010/10/08: C-c C-c is used in org-mode
  (define-key org-mode-map (kbd "C-M-x")   'org-toodledo-sync-task))

(defun org-configuration ()
  ;; Plato Wu,2010/08/29: use C-u C-c $ org-archive-subtree to archive DONE items
  (require 'org)
  (require 'org-archive)
  (setq org-log-done t)
  (setq org-log-into-drawer t)

  ;; Turn off prefer future dates 
  (setq org-read-date-prefer-future nil)

  (setq org-list-demote-modify-bullet (quote (("+" . "-")
                                              ("*" . "-")
                                              ("1." . "-")
                                              ("1)" . "-"))))
                                        ;disable priority commands.
  (setq org-enable-priority-commands nil)

  (setq org-agenda-files '("~/org/todo.org"))
  (define-key org-mode-map (kbd "C-M-j") 'org-insert-todo-heading)

  (setq org-modules 
        '(org-bbdb org-bibtex org-crypt org-gnus org-info
                   org-jsinfo org-inlinetask org-irc org-mew org-mhe org-vm org-wl org-w3m))

  (require 'org-crypt)
                                        ; Encrypt all entries before saving
  (org-crypt-use-before-save-magic)
  (setq org-tags-exclude-from-inheritance (quote ("crypt")))
                                        ; GPG key to use for encryption
  (setq org-crypt-key "E87C1128")

  (setq org-use-speed-commands t)




  (global-set-key "\C-cl" 'org-store-link)
  (global-set-key "\C-ca" 'org-agenda)
  (global-set-key "\C-cb" 'org-iswitchb)
  (setq org-return-follows-link t)

  (setq org-default-notes-file "~/org/todo.org")

  ;; enable task blocking
  (setq org-enforce-todo-dependencies t)

  ;; I use C-M-r to start capture mode
  (global-set-key (kbd "C-M-r") 'org-capture)

  ;; hides all blank lines inside folded contents of a tasks
  (setq org-cycle-separator-lines 0)

  (setq org-special-ctrl-a/e t)
  (setq org-special-ctrl-k t)
  (setq org-yank-adjusted-subtrees t)

                                        ;Non-nil means insert new headings after the current subtree.
  (setq org-insert-heading-respect-content t)

  ;; capture templates for TODO tasks
  (setq org-capture-templates 
        (quote (("t" "todo" entry (file+headline "~/org/todo.org" "Toodledo") "** TODO %?"))))

  (require 'org-publish)
  (add-to-list 'org-export-latex-packages-alist '("" "zhfontcfg" ))
  ;; Plato Wu,2011/02/17: protected all emphasis text for there is a bug
  ;; for text which contains number.
  (setq org-export-latex-emphasis-alist
  '(("*" "\\textbf{%s}" t)
    ("/" "\\emph{%s}" t)
    ("_" "\\underline{%s}" t)
    ("+" "\\st{%s}" t)
    ("=" "\\verb" t)
;    ("~" "\\verb" t)
    ;; Plato Wu,2011/02/18: use @ to tag Chinese characters for song font
    ;; if we use font as main font, the english font is ugly.
    ("~" "\\song{%s}" t )))
  ;; Plato Wu,2011/02/18: use org-export-as-pdf instead
  ;; (setq org-publish-project-alist
  ;;       '(  ("CoreBoardTesting"
  ;;            :base-directory "~/org/"
  ;;            :base-extension "org"
  ;;            :publishing-directory "~/org/public_pdf/"
  ;;            :recursive t
  ;;            :publishing-function org-publish-org-to-pdf
  ;;            :headline-levels 4             ; Just the default for this project.
  ;;            :auto-preamble t
  ;;            :auto-index t
  ;;            )
  ;;           ;; ... add all the components here (see below)...
  ;;           ))
  ;; Plato Wu,2011/04/22: use xelatext to do better with Chinese, and use system font.
  (setq org-latex-to-pdf-process '("xelatex -output-directory  log/ %s" 
                                   ;; moving intermediate tex file
                                   "mv `basename %b`.tex log/"
                                   ;; moving pdf for meeting org-export-as-pdf
                                   "mv log/`basename %b`.pdf ."))
  )
(org-configuration)

(defun sawfish-configuration ()
  (setq auto-mode-alist
      (append '(("\\.jl$" . sawfish-mode)
		("\\.sawfishrc$" . sawfish-mode)
		("\\.sawfish/rc$" . sawfish-mode))
	    auto-mode-alist)))
;; (define-key scheme-mode-map [f9] 'gds-show-last-stack)
;; (add-hook 'c-mode-hook '(lambda ()
;;                           (c-set-style "stroustrup")
;;                           (c-toggle-auto-hungry-state nil)))

(defun compile-configuration ()
  (global-set-key [f7] 'compile)
  ;;for waring from smart-compile
  (setq compilation-scroll-output t)
  (setq compilation-ask-about-save nil)
  (require 'gud)
  (gud-def gud-run "run" nil "start debug program")
  (gud-def gud-kill "kill" nil "kill debug program")
  (gud-def gud-y "y\n" nil "answer y for gdb")
  (defvar gdb-state nil "present gdb's state:nil, Start, Running, Stop")
  (defun gud-clear-break-list ()
    "Clear break point list"
    (interactive)
    (setq break-list nil))
  (defun gud-kill-9 ()
    "force to kill program which is debugging"
    (interactive)
    (let ((process-id
	   (string-to-int
	    (shell-command-to-string
	     (concat "ps -u $USER | grep " gdb-executable
		     " | gawk '{print $1}'")))))
      (if (and (equal gdb-state "Running")
	       (and  gud-last-last-frame
		     (not (eq gud-last-last-frame gdb-stop-p))))
	  (prog1
	      (gud-kill 1)
	    ;;must wait command's reponse
	    (sleep-for 1)
	    (gud-y 1))
	(if (= process-id 0)
	    (message "Program are not running")
	  (signal-process process-id 'SIGKILL)))
      (setq gdb-state nil
	    gdb-stop-p gud-last-last-frame)))
  (global-set-key (quote [27 f5]) (quote gud-kill-9))
  ;;judge whether gdb state is stop by gud-last-last-frame's change
  ;;but for gud-last-last-frame is also used by gud-refresh, so if user
  ;;run gud-refresh, the gdb's state will be confused
  (defvar gdb-stop-p nil "whether gdb's state is stop")
  (defvar gdb-executable nil "exetuable file for gdb")
  (defun onekey-debug ()
    "Save buffers and start debug"
    (interactive)
    (save-some-buffers t)
    (cond ((equal gdb-state nil)
	   (setq gdb-state "Start")
	   ;;delete compilation window
	   (delete-other-windows)
	   (split-window-vertically)
	   (other-window 1)
	   ;;use specified project
					;	 (gdb "gdb --cd=~/PMP/UI/Build PMP")
	   (find-file "Makefile")
	   ;;	 (set-buffer "Makefile")
	   (setq gdb-executable
		 (shell-command-to-string
		  "gawk '/^EXECUTABLE/{print $3}' Makefile"))
	   (gdb (concat "gdb " gdb-executable))
	   ;;return source window
	   (other-window 1))

	  ((equal gdb-state "Start")
	   (setq gdb-state "Running"
		 gdb-stop-p gud-last-last-frame)
	   (gud-run 1))
	  ((equal gdb-state "Running")
	   (if (and gud-last-last-frame
		    (not (eq gud-last-last-frame gdb-stop-p)))
	       (progn (gud-cont 1)
		      (setq gdb-stop-p gud-last-last-frame))
;;;To Debug
	     (let ((process-id
		    (string-to-int
		     (shell-command-to-string
		      (concat "ps -u $USER | grep " gdb-executable
			      " | gawk '{print $1}'")))))
	       (if (= process-id 0)
		   (message "Program are not running")
		 (signal-process process-id 'SIGINT))
	       (if gud-last-last-frame (setq gdb-stop-p nil)))))))
  (defvar break-list nil "a list which stores break lines")
					;use memq instead of element-in-list-p?
  (defun element-in-list-p (element list)
    "judge whether element is in the list"
    (if list
	(if (equal element (car list))
	    element
	  (element-in-list-p element (cdr list)))
      nil))

  (defun gud-break-remove ()
    "set breakpoint or remove a existed one"
    (interactive)
    (if gdb-state
	(let ((element (list buffer-file-name (what-line))))
	  (if (element-in-list-p element break-list)
					;delq use eq so it can not be used here. we must use delete
	      (progn (setq break-list (delete element break-list))
		     (gud-remove 1))
	    (progn (setq break-list (cons element break-list))
		   (gud-break 1))))
      (gud-break 1)))
  (defun gud-evaluation ()
    "if emacs is at debug state, then evaluation C expression at point,
else evaluate sexp"
    (interactive)
    (if gdb-state
	(gud-print 1)
      (eval-last-sexp nil)))
  ;;overload C-x C-e
  (global-set-key "" 'gud-evaluation)
  ;;overload "C-x "
  (global-set-key " "  'gud-break-remove)
  (global-set-key [f5] 'onekey-debug)
  ;;(global-set-key [f9] 'gud-break)
  ;; f10 is menu key but M-` is also menu key
  (global-set-key [f10] 'gud-next)
  (global-set-key [f11] 'gud-step)
  (setq gdb-many-windows t))

(compile-configuration)

(defun session-configuration ()
  (add-hook 'after-init-hook 'session-initialize))

(defun slime-configuaration ()
  (setq auto-mode-alist
      (append '(("\\.sbclrc$" . lisp-mode))
	    auto-mode-alist)))

(defun w3m-configuration ()
  (setq w3m-default-coding-system 'utf-8)
  (setq browse-url-browser-function 'w3m-browse-url)
  ;; Plato Wu,2010/01/26: supress message: ImageMagick's `convert' program is not available
  (setq w3m-use-favicon nil)
  (setq w3m-imagick-convert-program nil)
  (setq w3m-use-cookies t)
   ;; Plato Wu,2010/03/28: google.com is censored, disable this function for
   ;; w3m-goto-url
  (setq w3m-enable-google-feeling-lucky nil)
  (add-hook 'w3m-mode-hook 
             '(lambda ()
                (w3m-link-numbering-mode 1)))
  
  (defun my-w3m-href-text (&optional position)
     "Return text linked up with the href anchor at the given POSITION.
NOTE: If POSITION is omitted, it searches for the property in one
character before and behind the current position, and point will move
to the position where the property exists."
     (when (if position
	       (get-text-property position 'w3m-href-anchor)
	     (w3m-get-text-property-around 'w3m-href-anchor))
       (w3m-replace-in-string
	(buffer-substring-no-properties
	 (if (or (bobp)
		 (not (get-text-property (1- (point)) 'w3m-href-anchor)))
	     (point)
	   (previous-single-property-change (point) 'w3m-href-anchor))
	 (next-single-property-change (point) 'w3m-href-anchor))
	" +\\'" ""))))

(provide 'my-packages)

  