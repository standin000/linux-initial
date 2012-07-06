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
      '(("rm" "~/linux-initial/shell/movetotrash.sh $*")
        ;; Plato Wu,2010/06/23: /bin don't work for Emacs in Windows
	("del" "/bin/rm -i -f $*")
;	("cp" "/bin/cp -i $*")
        ;; Plato Wu,2010/09/13: rm -i cp -i don't work for Emacs in Linux
;	
	("cp" "/bin/cp -p -i $*")
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
        ("curl" "curl -C - -O $*")   ;resume download and use remote file name
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

;; (load "dictionary-init.el")
;; ;; Plato Wu,2010/05/29: '[C-M-z]
;; (global-set-key "" 'dictionary-search)
;; (setq dictionary-use-single-buffer t)
;; ;; Plato Wu,2010/10/17: this site use international phonetic alphabet
;; (setq dictionary-server "dict.hewgill.com")

(defun ido-configuration ()
  ;; Plato Wu,2011/08/13: 
  (setq ido-enable-tramp-completion t) 
;; Plato Wu,2009/06/04: If it is mess, try to use ido-wash-history 
  (setq ido-ignore-buffers
	'("^ .*"
	;; ignore *eshell*, *svn-status*, a awkward regular expression
	;; for I do not know how to perfect match a word which is not "eshell"
	;; or "svn-status", magit:. "*terminal" for multi-terminal
	  "^\\*[^esmt].\\{3\\}[^s].*"
	  "TAGS"))
  (setq ido-record-ftp-work-directories nil)
  ;; Plato Wu,2011/06/08: ignore Too big for folder whose size >116k
  ;; since folder size expand when number of files increase but
  ;; don't shrink after delete file.
  (setq ido-max-directory-size 118785)

;; Plato Wu,2009/06/04: let ido-work-directory-list not record /sudo
;; and /ssh so that ido do not need wait 60s for visit /sudo or /ssh
  (setq ido-work-directory-list-ignore-regexps '("/sudo:" "/ssh:"))
  ;; Plato Wu,2008/12/09: remarks for key is conflict with redshank mode
  (define-key ctl-x-map "\C-r" nil)
  (ido-mode t)
  (ido-hacks-mode t)
  )

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

;; use "r" to modify file name in dired mode, It need
;; C-c C-c to commit final name.
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

(setq ibuffer-default-sorting-mode 'major-mode)

(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("dired" (mode . dired-mode))
               ("perl" (mode . cperl-mode))
               ("erc" (mode . erc-mode))
               ("emacs" (or
                         (name . "^\\*scratch\\*$")
                         (name . "^\\*Messages\\*$")))
               ("gnus" (or
                        (mode . message-mode)
                        (mode . bbdb-mode)
                        (mode . mail-mode)
                        (mode . gnus-group-mode)
                        (mode . gnus-summary-mode)
                        (mode . gnus-article-mode)
                        (name . "^\\.bbdb$")
                        (name . "^\\.newsrc-dribble")))))))

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "default")))

(setq ibuffer-maybe-show-predicates 
      `(,(lambda (buf)
           (and (and (string-match "^ " (buffer-name buf))
                 (null buffer-file-name))
               ;; Plato Wu,2011/05/22: show http buffer of url-request-data for debug syncml
               (not (string-match "^ \\*http" (buffer-name buf)))))))

(defun emms-configuration ()
  (require 'emms-setup)
  (require 'emms-player-mpd)
  (require 'emms-history)
  (require 'emms-score)
  (require 'emms-i18n)
  (require 'emms-volume)
  (require 'emms-lyrics)
  ;; Plato Wu,2011/05/15: emms-standard will require emms-source-file and emms-source-playlist.
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
  ;; Plato Wu,2011/05/15: it seems mpd doesn't support emms-random
  ;; use emms-shuffle instead which  will shuffle the playlist
  ;;  (add-hook 'emms-player-finished-hook 'emms-random)
  ;;  (setq emms-player-next-function 'emms-random)
  (setq emms-playlist-buffer-name "*Music*")
  (setq emms-player-mpd-music-directory "~/Music")
  (setq emms-player-next-function 'emms-next-noerror)
  (when (is-system "windows-nt")
    (add-to-list 'exec-path "d:/Tools/MPlayer/")
    (custom-set-variables
     '(emacsw32-style-frame-title t)
     '(w32shell-cygwin-bin "d:\\Tools\\Cygwin\\bin")
     '(w32shell-shell (quote cygwin)))
    ;; Plato Wu,2010/04/07: make sure default-file-name-coding-system  and default-process-coding-system is OK
    (setq default-process-coding-system (cons 'cp936 'cp936)))

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
                                        ;  (emms-history-load)
  (setq emms-lyrics-dir "~/Music/Lyrics")
  (setq emms-lyrics-coding-system 'gbk-dos)
                                        ;    (setq emms-lyrics-display-on-minibuffer t)
  (emms-lyrics 1)
  (defadvice gnus-group-get-new-news (around pause-emms)
    "Pause emms while Gnus is fetching mails or news."
    (if emms-player-playing-p
        (progn (emms-pause)
               ad-do-it
               (emms-pause))
      ad-do-it))
  (ad-activate 'gnus-group-get-new-news)
  ;; global key-map
  ;; all global keys prefix is C-c e
  ;; compatible with emms-playlist mode keybindings
  ;; you can view emms-playlist-mode.el to get details about 
  ;; emms-playlist mode keys map
  (global-set-key (kbd "C-c e s") 'emms-stop)
  (global-set-key (kbd "C-c e P") 'emms-pause)
  (global-set-key (kbd "C-c e n") 'emms-next)
  (global-set-key (kbd "C-c e p") 'emms-previous)
  (global-set-key (kbd "C-c e f") 'emms-show)
  (global-set-key (kbd "C-c e >") 'emms-seek-forward)
  (global-set-key (kbd "C-c e <") 'emms-seek-backward)
  ;; these keys maps were derivations of above keybindings
  (global-set-key (kbd "C-c e S") 'emms-start)
  (global-set-key (kbd "C-c e g") 'emms-playlist-mode-go)
  (global-set-key (kbd "C-c e t") 'emms-play-directory-tree)
  (global-set-key (kbd "C-c e h") 'emms-shuffle)
  (global-set-key (kbd "C-c e e") 'emms-play-file)
  (global-set-key (kbd "C-c e l") 'emms-play-playlist)
  (global-set-key (kbd "C-c e r") 'emms-toggle-repeat-track)
  (global-set-key (kbd "C-c e R") 'emms-toggle-repeat-playlist)
  (global-set-key (kbd "C-c e u") 'emms-score-up-playing)
  (global-set-key (kbd "C-c e d") 'emms-score-down-playing)
  (global-set-key (kbd "C-c e o") 'emms-score-show-playing)    
  )

(defvar my-authinfo "~/.authinfo")

(defun muse-configuration ()
)

(defun auctex-configuration ()
  (require 'preview-latex)
  (setq TeX-auto-save t)
  (setq Te-parse-self t)
  (setq-default TeX-master nil)
  (setq TeX-save-query nil)
  ;; (add-hook 'LaTeX-mode-hook 'TeX-PDF-mode) ;turn on pdf-mode
  ;; (remove-hook 'LaTeX-mode-hook 'TeX-PDF-mode)
  ;;Plato 2008/3/10》 : write a procedure like C-c C-c but no need press enter key ; be testing.
  ;; Plato Wu,2008/11/26, it just works for compiling not for viewing, use C-c C-v for viewing.
  (defun Test-command-master (&optional override-confirm)
    "Run command on the current document.
    If a prefix argument OVERRIDE-CONFIRM is given, confirmation will
    depend on it being positive instead of the entry in `TeX-command-list'."
    (interactive "P")
    (TeX-command (Test-command-query (TeX-master-file)) 'TeX-master-file
		 override-confirm))

  (defun Test-command-query (name)
    "Query the user for what TeX command to use."
    (let* ((default (cond ((if (string-equal name TeX-region)
			       (TeX-check-files (concat name "." (TeX-output-extension))
						(list name)
						TeX-file-extensions)
			     (TeX-save-document (TeX-master-file)))
			   TeX-command-default)
			  ((and (memq major-mode '(doctex-mode latex-mode))
				(TeX-check-files (concat name ".bbl")
						 (mapcar 'car
							 (LaTeX-bibliography-list))
						 BibTeX-file-extensions))
			   ;; We should check for bst files here as well.
			   TeX-command-BibTeX)
			  ((TeX-process-get-variable name
						     'TeX-command-next
						     TeX-command-Show))
			  (TeX-command-Show)))
	   (completion-ignore-case t)
	   ;; 	 (answer (or TeX-command-force
	   ;; 		     (completing-read
	   ;; 		      (concat "Command: (default " default ") ")
	   ;; 		      (TeX-mode-specific-command-list major-mode) nil t
	   ;; 		      nil 'TeX-command-history)))
	   )
      ;; If the answer "latex" it will not be expanded to "LaTeX"
      ;;    (setq answer (car-safe (TeX-assoc answer TeX-command-list)))
      ;;    (if (and answer
      ;;	     (not (string-equal answer "")))
      ;;	answer
      default))
;  (global-set-key [f9] 'Test-command-master)
  )
;; Plato Wu,2011/05/19: we must change the usage of el-get, don't use :after option to
;; call package configuration, but run it after el-get finished its task. since el-get
;; use eval-after-load to call :after function, it is annoy style.
;(auctex-configuration)

(defun blogger-configuration ()
  (require 'muse) 
  (require 'muse-mode) 
  (require 'muse-publish) 
  (require 'muse-html)
  (require 'weblogger)
  (add-to-list 'auto-mode-alist '("\\.muse$" . muse-mode))
  (define-key muse-mode-map "u" 'muse-insert-url)
  (define-key muse-mode-map "l"
    '(lambda ()
       (interactive)
       (insert "<literal style=\"blogger\">\n\n</img>\n</a>\n</literal>")
       (previous-line 3)))
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
           (insert (format "<p>....</p><p>请移步<a href=\"http://platowu.info\">恒永之地</a>查看<a href=\"http://platowu.info/%s\"><font size = 6 color=\"#FF0000\">点我</font></a>，评论也在那边留哦，:)</p>" 
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
  (require 'org-mobile)
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

  (setq org-agenda-files '("~/org/agendas.org"))
  (setq org-mobile-directory "~/backup/backup/Dropbox/MobileOrg")

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

(defun sawfish-configuration ()
  (setq auto-mode-alist
      (append '(("\\.jl$" . sawfish-mode)
		("\\.sawfishrc$" . sawfish-mode)
		("\\.sawfish/rc$" . sawfish-mode))
	    auto-mode-alist)))
;; (when window-system
;;   (unless (or (string= system-type "darwin") (string= system-type "windows-nt"))
;;     (add-hook 
;;      'kill-emacs-hook 
;;      '(lambda ()
;; 	(sawfish-eval "(quit)"))
;;      t)))
;; (define-key scheme-mode-map [f9] 'gds-show-last-stack)
;; (add-hook 'c-mode-hook '(lambda ()
;;                           (c-set-style "stroustrup")
;;                           (c-toggle-auto-hungry-state nil)))

(defun compile-configuration ()
  (global-set-key [f7] 'compile)
  (setq compilation-read-command nil)
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

(defun slime-configuration ()
  (load (expand-file-name "~/quicklisp/slime-helper.el"))
  ;; Replace "sbcl" with the path to your implementation
  (setq inferior-lisp-program "sbcl")
  ;; Plato Wu,2011/05/16: it will report different version between slime and
  ;; swank when slime get install after swank. it seems as a bug.
  ;; Plato Wu,2011/05/24: it seems I use two slime of different version
;  (setq slime-protocol-version 'ignore)
  (setq auto-mode-alist
        (append '(("\\.sbclrc$" . lisp-mode))
                auto-mode-alist))
  (setq slime-net-coding-system 'utf-8-unix)
  ;; slime-fancy is the meta contrib that will enable all sorts of advanced features.
  ;; Plato Wu,2008/10/28, slime-highlight-edits cause a non-convenient color in text-mode.
  ;; Plato Wu,2008/10/17, slime-typeout-frame will cause new frame for typeout
  ;; Plato Wu,2008/12/18: it seems I have not slime-indent contrib
  ;; which can resolve emacs loop indent problem.

  (slime-setup '(slime-fancy slime-tramp slime-asdf slime-repl
                             slime-fancy-inspector slime-autodoc slime-fuzzy slime-presentation-streams))
  (add-hook 'lisp-mode-hook (lambda () (slime-mode t)))
  ;; Plato Wu,2009/12/12: temperate clear lisp connection closed unexpectedly
  ;; problem
  (defun load-swank-dont-close (port-filename encoding)
    (format "%S\n\n"
            `(progn
               (load ,(expand-file-name slime-backend slime-path) :verbose t)
               (funcall (read-from-string "swank-loader:init"))
               (funcall (read-from-string "swank:start-server")
                        ,port-filename
                        :coding-system ,(slime-coding-system-cl-name encoding)
                        :dont-close t))))
  ;; Plato Wu,2009/12/09: clear content at the bottom of the screen
  (defun slime-quit-sentinel (process message)
    (assert (process-status process) 'closed)
    (let* ((inferior (slime-inferior-process process))
           (inferior-buffer (if inferior (process-buffer inferior))))
      (when inferior (delete-process inferior))
      (when inferior-buffer (kill-buffer inferior-buffer))
      (slime-net-close process)
      ;;    (message "Connection closed.")
      ))

  (setq slime-lisp-implementations
        '((sbcl-noclose ("sbcl" "-quiet") :init load-swank-dont-close)))

  ;; Plato Wu,2009/11/05: TO DO, assign a proper key binding to slime-repl-backward-input
  ;;   (define-key slime-repl-mode-map (kbd "M-<up>") 'slime-repl-backward-input)
  ;; Plato Wu,2009/05/27: C-c C-k will show compliation log
  (setq slime-compilation-finished-hook 'slime-show-compilation-log)
  (defun slime-repl-history-pattern (&optional use-current-input)
    "Return the regexp for the navigation commands."
    (cond ((slime-repl-history-search-in-progress-p)
           slime-repl-history-pattern)
          (use-current-input
           (let ((str (slime-repl-current-input)))
             (cond ((string-match "^[ \n]*$" str) nil)
					;(t (concat "^" (regexp-quote str)))
                   ;; Plato Wu,2009/03/25: because I used paredit so if I
                   ;; enter '(', ')' will entered automatically, so the partial
                   ;; search fail, if I do not enter '(', '^' also cause it fail.
                   (t (regexp-quote str))
                   )))
          (t nil)))
  (setq slime-startup-animation nil)
  ;; Plato Wu,2008/10/17, It will cause new frame for sldb
  ;; (setq special-display-regexps
  ;;       (quote (("slime-repl" (height . 40) (width . 80) (top . 85) (left . 50))
  ;; 	      ("sldb" (height . 30) (width . 50) (left . 10) (top . 25)))))
  ;; Plato Wu,2011/05/18: it will run slime-load-hook which contain slime-setup-contribs.
  (require 'info-look)
  (mapc (lambda (mode) 
          (info-lookup-add-help
           :mode mode
           :regexp "[^][()'\" \t\n]+"
           :ignore-case t
           :doc-spec '(("(ansicl)Symbol Index" nil nil nil))))
        '(lisp-mode slime-repl-mode))
  (defun cl-info (symbol-name)
    (interactive
     ;; Plato Wu,2009/03/29: new slime.el rename `slime-symbol-name-at-point' to
     ;; `slime-symbol-at-point',
     (list (let* ((symbol-at-point (slime-symbol-at-point))
                  (stripped-symbol 
                   (and symbol-at-point
                        (downcase
                         (common-lisp-hyperspec-strip-cl-package 
                          symbol-at-point)))))
             (if (and stripped-symbol
                      (intern-soft stripped-symbol
                                   common-lisp-hyperspec-symbols))
                 stripped-symbol
               (completing-read
                "Look up symbol in Common Lisp HyperSpec: "
                common-lisp-hyperspec-symbols #'boundp
                t stripped-symbol
                'common-lisp-hyperspec-history)))))
    (info-lookup 'symbol symbol-name nil)
    (other-window 1))
  (eval-after-load "lisp-mode"
    '(progn
       (define-key 
         lisp-mode-map 
         [(control ?h) (control ?i)] 
         'cl-info)))
  ;; Plato Wu,2008/12/18: reslove loop indent'problem. 
  (setq lisp-simple-loop-indentation 1
	lisp-loop-keyword-indentation 6
	lisp-loop-forms-indentation 6)
  (defun isearch-yank-symbolic-word-or-char ()
    (interactive)
    (isearch-yank-internal
     (lambda ()
       (let ((distance (skip-syntax-forward "w_")))
         (when (zerop distance) (forward-char 1))
         (point)))))

  (add-hook 'lisp-mode-hook
            (lambda ()
              (make-local-variable 'isearch-mode-map)
              (define-key isearch-mode-map "\C-w" 'isearch-yank-symbolic-word-or-char)
                                        ;The Emacs default indentation for some forms such as `if' is likely to make CommonLisp hackers unhappy. Emacs also provides a `common-lisp-indent-function', but it's not enabled by default.
              (set (make-local-variable 'lisp-indent-function)
                   'common-lisp-indent-function)))
  (require 'slime)
  (defun quit-slime ()
    (interactive)
    (when (slime-connected-p)
      (if (buffer-exist-p "*inferior-lisp*")
          (progn 
            (slime-quit-lisp t)
            (sleep-for 6))
        ;; Plato Wu,2010/08/19: if it is stumpwm, then disconnect it.
        (slime-disconnect-all))
      (slime-kill-all-buffers)))
  (define-key slime-mode-map "\M-*" 'slime-pop-find-definition-stack)
  ;; Plato Wu,2009/07/03: redefine it to quit and kill the buffer!
  (define-key slime-popup-buffer-mode-map "q" #'(lambda () (interactive) (slime-popup-buffer-quit-function t)))
  ;; Plato Wu,2009/05/26: use slime-fuzzy-complete-symbol instead
  ;; normal C-tab function.
  (define-key slime-mode-map "\C-\M-i" 'slime-fuzzy-complete-symbol)
  (define-key slime-repl-mode-map "\C-\M-i" 'slime-fuzzy-complete-symbol))

(when (file-exists-p "~/quicklisp/slime-helper.el")
    (slime-configuration)) 

(defun cldoc-configuration ()
  (autoload 'turn-on-cldoc-mode "cldoc" nil t)
  (add-hook 'lisp-mode-hook 'turn-on-cldoc-mode)
  ;;
  ;; ;; ilisp users
  ;; (add-hook 'ilisp-mode-hook 'turn-on-cldoc-mode)
  ;; (setq ilisp-bindings-*bind-space-p* nil)
  ;; 
  ;; slime users
  (add-hook 'slime-repl-mode-hook
            #'(lambda ()
                (turn-on-cldoc-mode)
                (define-key slime-repl-mode-map " " nil)))
  (add-hook 'slime-mode-hook
            #'(lambda () (define-key slime-mode-map " " nil)))
  (setq slime-use-autodoc-mode nil))

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

(defun ascii-configuration ()
  (autoload 'ascii-on "ascii" "Turn on ASCII code display." t))

;; (if (try-require 'jabber)
;;     (let ((netrc-data (netrc-machine (netrc-parse my-authinfo) "talk.google.com" "ssl")))
;;         (setq jabber-account-list
;;               `((,(cdr (assoc "login" netrc-data)) 
;;                  (:network-server . "talk.google.com")
;;                  (:password . ,(cdr (assoc "password" netrc-data)))
;;                  (:connection-type . ssl))))
;;         (define-key jabber-chat-mode-map (kbd "M-RET") 'newline)))

;; (redshank-setup '(lisp-mode-hook
;;                         slime-repl-mode-hook
;; 			emacs-lisp-mode-hook) t)

(provide 'my-packages)

  