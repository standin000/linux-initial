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
               (concat (concat "..." (substring pwd 20))
                       (if (= (user-uid) 0) " # " " $ ")))))))
                                        ;remove duplicate history items.
  (setq eshell-hist-ignoredups t)
  ;; ;; Plato Wu,2009/04/10: it seems there is not effect in emacs 22.2.1 To check
  ;; ;; let eshell show color but it is slow.
  ;; (require 'ansi-color)
  ;; (add-hook 'eshell-preoutput-filter-functions
  ;;           'ansi-color-apply)
  ;; (autoload 'ansi-color-apply "ansi-color" nil t)
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

  ;; Plato Wu,2012/07/26: set it nil it will use envvar HISTSIZE, but (getenv "HISTSIZE") return a string, make-ring report a error.
  (setq eshell-history-size 2000)
  ;; (setq eshell-history-ring nil)
  ;; remove bad character in .bash_history to resolve coding system problem when exit eshell
  ;; Plato Wu,2013/01/25: eshell will overwrite bash command history which is invoked during
  ;; eshell running timeframe.
  ;; Plato Wu,2013/07/24: if .bash_history is mess, clear it when emacs is closed
  (setq eshell-history-file-name (expand-file-name "~/.bash_history"))
  ;; Plato Wu,2014/02/13: If not, insert .bash_history will bring \d
  (modify-coding-system-alist 'file "\\.bash_history\\'" 'utf-8-dos)
  (defun eshell-update-history ()
    (let ((file eshell-history-file-name))
      (cond
       ((or (null file)
            (equal file ""))
        nil)
       ((not (file-readable-p file))
        (or silent
            (message "Cannot read history file %s" file)))
       (t
        (let* ((count 0)
               ;; Plato Wu,2013/06/21: because it is difficult to accurate new number of
               ;; .bash_history after starting eshell, it assume 222 is OK.
               ;; @todo need a accurate number
               (size 22) 
               ;; Plato Wu,2013/06/21: temp buffer will use its own eshell variable
               (ring eshell-history-ring)
               (ignore-dups eshell-hist-ignoredups))
          (with-temp-buffer
            (insert-file-contents file)
            ;; Save restriction in case file is already visited...
            ;; Watch for those date stamps in history files!
            (goto-char (point-max))
            (forward-line (* -1 size))
            (while (and (< count size)
                        (re-search-forward "^[ \t]*\\([^#\n].*\\)[ \t]*$"
                                            nil t))
              (let ((history (match-string 1)))
                (if (or (null ignore-dups)
                        (ring-empty-p ring)
                        (not (string-equal (ring-ref ring 0) history)))
                    (ring-insert
                     ring (subst-char-in-string ?\177 ?\n history))))
              (setq count (1+ count)))))))))
  (add-hook 'eshell-mode-hook
            #'(lambda () 
                (define-key eshell-mode-map "\C-a" 'eshell-maybe-bol)
                ;; Plato Wu,2013/06/21: it must be defined here, or this file will new
                ;; its own variabile for eshell-exit-hook
                (add-hook 'eshell-exit-hook 'eshell-update-history nil t)))
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
          ;; Plato Wu,2010/09/13: rm -i cp -i don't work for Emacs in Linux
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
    ;; (setcdr (assoc "rm" eshell-command-aliases-list) '("rm -i -f"))
    (setcdr (assoc "del" eshell-command-aliases-list) '("rm -i -f $*"))))

(eshell-configuration)

(defun magit-configuration ()
  ;; Plato Wu,2016/01/25: @todo magit-push can not enter password for https link
  ;; Plato Wu,2016/01/22: M-h & M-H to show only file(s)
;  (require 'magit)
  (global-set-key (kbd "C-x C-z") 'magit-status)
  ;; Plato Wu,2015/06/01: stop 1.4.0 auto revert warning.
  (setq magit-auto-revert-mode nil)
  (setq magit-last-seen-setup-instructions "1.4.0")
  )

(defun paredit-configuration ()
  ;; Plato Wu,2008/09/27, paredit will cause alt-s, ctrl-d can not used in ido mode
;;  (add-hook 'minibuffer-setup-hook #'(lambda () (paredit-mode +1)))
;; Plato, 08/09/04, fundamental mode is a base mode, so it doesn't have
;; a mode hook, it only run change-major-mode-hook but enter others mode
  ;; should enter fundamental mode first, so this hook would be run always.
;  (require 'paredit)
  (mapc #'(lambda (mode)
	  (let ((hook (intern (concat (symbol-name mode)
				      "-mode-hook"))))
	    (add-hook hook #'(lambda () (paredit-mode +1)))))
	'(emacs-lisp lisp slime-repl scheme ielm))
  (defadvice ielm-eval-input (after ielm-paredit activate)
    "Begin each IELM prompt with a ParEdit parenthesis pair."
    (paredit-open-round))
  ;; Plato Wu,2009/12/09: enable eldoc mode.
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode))

;; (load "dictionary-init.el")
;; ;; Plato Wu,2010/05/29: '[C-M-z]
;; (global-set-key "" 'dictionary-search)
;; (setq dictionary-use-single-buffer t)
;; ;; Plato Wu,2010/10/17: this site use international phonetic alphabet
;; (setq dictionary-server "dict.hewgill.com")

(defun ido-configuration ()
  ;; Plato Wu,2013/06/07: why set it t? to lookup remote path
;; Plato Wu,2014/05/02: 
;;  (setq ido-enable-tramp-completion nil) 
;; Plato Wu,2009/06/04: If it is mess, try to use ido-wash-history and set ido-work-file-list to nil
;; Plato Wu,2013/07/05: if .ido.last is mess or bring sudo buffer at the beginning, 
;; clear it when emacs is closed
;; Plato Wu,2014/04/28: use new method to get rid of sudo buffer
;;  (require 'tramp-cmds)
;;  (add-hook 'desktop-save-hook 'tramp-cleanup-all-buffers)
  (setq ido-ignore-buffers
	'("^ .*"
	;; ignore *eshell*, *svn-status*, a awkward regular expression
	;; for I do not know how to perfect match a word which is not "eshell"
	;; or "svn-status", magit:. "*terminal" for multi-terminal
	  "^\\*[^esmti].\\{3\\}[^s].*"
	  "^TAGS$"
      ;; "^/sudo.*"
      ;; "^ssh.*"
      ;; "^scp.*"
      "^\\*tramp.*"
      ))
  (setq ido-record-ftp-work-directories nil)
  ;; Plato Wu,2011/06/08: ignore Too big for folder whose size >116k
  ;; since folder size expand when number of files increase but
  ;; don't shrink after delete file.
  (setq ido-max-directory-size 118785)

;; Plato Wu,2009/06/04: let ido-work-directory-list not record /sudo
;; and /ssh so that ido do not need wait 60s for visit /sudo or /ssh
  (setq ido-work-directory-list-ignore-regexps '("^/sudo:.*" "^/ssh:.*" "^/scp.*"))
  ;; Plato Wu,2008/12/09: remarks for key is conflict with redshank mode
  (define-key ctl-x-map "\C-r" nil)
  (setq ido-use-virtual-buffers t)
  (setq ido-enable-flex-matching t)
  ;; Plato Wu,2014/02/09: disable it temperately
;;  (setq ido-everywhere t)
  ;; Plato Wu,2013/06/07: it is annoy when current-word is meaningless path.
  (setq ido-use-filename-at-point nil)
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
    #'(lambda ()
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

;(setq ibuffer-default-sorting-mode 'major-mode)

(setq ibuffer-maybe-show-predicates 
      `(,(lambda (buf)
           (and (and (string-match "^ " (buffer-name buf))
                 (null buffer-file-name))
               ;; Plato Wu,2011/05/22: show http buffer of url-request-data for debug syncml
                (not (string-match "^ \\*http" (buffer-name buf)))))))

;; Plato Wu,2015/04/13: there is helm-configuration first.
(defun helm-config ()
  (require 'helm-config)
  (helm-mode 1)
;  (require 'projectile)
  ;; If you don't want the Helm window to be resized, but a smaller Helm window, you can set helm-autoresize-max-height equal to helm-autoresize-min-height.

  ;  (helm-autoresize-mode t)

;; If you use golden-ratio, you have to disable its interference with Helm window:

;; (defun pl/helm-alive-p ()
;;   (if (boundp 'helm-alive-p)
;;       (symbol-value 'helm-alive-p)))

;; (add-to-list 'golden-ratio-inhibit-functions 'pl/helm-alive-p)

  ;helm-ff-transformer-show-only-basename nil
  ;helm-adaptive-history-file             "~/.emacs.d/helm-history"
  ;helm-ff-auto-update-initial-value      t
  (setq helm-yank-symbol-first                t
;       it can not jump between buffers and recentf
;        helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
        helm-buffers-fuzzy-matching           t ; fuzzy matching buffer names when non--nil
        helm-recentf-fuzzy-match              t
        helm-quick-update                     t ; do not display invisible candidates
        helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
        helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
        helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
        helm-ff-file-name-history-use-recentf t
        helm-M-x-fuzzy-match                  t
        )
  ;; (defun helm-find-files-sensitive-backspace ()
  ;;   "Deletes whole directory in helm find files mode on backspace."
  ;;   (interactive)
  ;;   (if (char-equal ?/ (char-before))
  ;;       (helm-find-files-up-one-level 1)
  ;;     (backward-delete-char 1)))
  ;; ;; helm better navigation
  ;; (define-key helm-find-files-map (kbd "<backspace>") 'helm-find-files-sensitive-backspace)
  ;; (define-key helm-find-files-map (kbd "<DEL>") 'helm-find-files-sensitive-backspace)
  ;; Plato Wu,2015/04/20: <DEL> is used to del character too.
  ;;  (define-key helm-find-files-map (kbd "<DEL>") 'helm-find-files-up-one-level)
  ;; (define-key helm-find-files-map (kbd "C-h") 'helm-find-files-sensitive-backspace)
  ;; (define-key helm-map (kbd "C-h") 'delete-backward-char)
  ;; Plato Wu,2016/09/08: it report errors when hit tab at C-x b
  (defun fu/helm-find-files-navigate-forward (orig-fun &rest args)
    (if (file-directory-p (helm-get-selection))
        (apply orig-fun args)
      (helm-exit-minibuffer)))
  (advice-add 'helm-execute-persistent-action :around #'fu/helm-find-files-navigate-forward)
;  (define-key helm-find-files-map (kbd "RET") 'helm-execute-persistent-action)
  (defun fu/helm-find-files-navigate-back (orig-fun &rest args)
    (if (= (length helm-pattern) (length (helm-find-files-initial-input)))
        (helm-find-files-up-one-level 1)
      (apply orig-fun args)))
  (advice-add 'helm-ff-delete-char-backward :around #'fu/helm-find-files-navigate-back)
  (setq helm-ff-skip-boring-files t)
  ;; (autoload 'helm-descbinds      "helm-descbinds" t)
  ;; (autoload 'helm-eshell-history "helm-eshell"    t)
  ;; (autoload 'helm-esh-pcomplete  "helm-eshell"    t)
  (global-set-key (kbd "C-h a")    #'helm-apropos)
  (global-set-key (kbd "C-h i")    #'helm-info-emacs)
  (global-set-key (kbd "C-h b")    #'helm-descbinds)

  (global-set-key (kbd "C-x C-f")  #'helm-find-files)
  (global-set-key (kbd "C-x b") #'helm-mini)
;  (global-set-key (kbd "C-x b") 'projectile-project-buffers-other-buffer)

;  (global-set-key (kbd "C-x C-m") #'helm-M-x)
;  (global-set-key (kbd "C-x C-f") #'helm-find-files)
  (global-set-key (kbd "C-x C-r") #'helm-recentf)
  (global-set-key (kbd "C-x r l") #'helm-filtered-bookmarks)
;  (global-set-key (kbd "M-y")     #'helm-show-kill-ring)
  (global-set-key (kbd "M-s o")   #'helm-swoop)
  (global-set-key (kbd "M-s /")   #'helm-multi-swoop)
  ;(global-set-key (kbd "M-x") 'helm-M-x)

  (global-set-key (kbd "C-x c!")  #'helm-calcul-expression)
  (global-set-key (kbd "C-x c:")  #'helm-eval-expression-with-eldoc)

  (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)  ; make TAB works in terminal

  (define-key helm-map (kbd "M-o") #'helm-previous-source)

  (define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

  (when (executable-find "curl")
    (setq helm-google-suggest-use-curl-p t))

  (global-set-key (kbd "M-s s")   #'helm-ag)

  (define-key projectile-mode-map (kbd "C-c p /")
    #'(lambda ()
        (interactive)
        (helm-ag (projectile-project-root))))

;;  (define-key org-mode-map (kbd "C-x c o h") #'helm-org-headlines)

  (setq projectile-completion-system 'helm)
  ;; Plato Wu,2015/03/20: don't use helm for describe function and variable.
  (setq helm-completing-read-handlers-alist '((describe-function . nil)
                                              (describe-variable . nil)
                                              (debug-on-entry . helm-completing-read-symbols)
                                              (find-function . helm-completing-read-symbols)
                                              (find-tag . helm-completing-read-with-cands-in-buffer)
                                              (ffap-alternate-file)
                                              (tmm-menubar))))

(defun helm-projectile-configuration ()
;  (require 'helm-projectile)
  ;; Plato Wu,2015/04/30: no suitable switch projectile buffer function
  (global-set-key (kbd "C-x n") #'helm-projectile-find-file)
  (setq helm-projectile-sources-list (cons 'helm-source-projectile-files-list
                                           (remove 'helm-source-projectile-files-list
                                                   helm-projectile-sources-list)))
  ;  (global-set-key (kbd "C-x b") 'helm-projectile-switch-to-buffer)
  (helm-projectile-on)
  )


(defun projectile-configuration ()
;  (require 'projectile)
  (projectile-global-mode)
  (setq projectile-globally-ignored-file-suffixes '("~"))
  ;; (global-set-key [remap find-file] 'projectile-find-file)
  ;; (global-set-key [remap switch-to-buffer] 'projectile-switch-to-buffer)
  (setq projectile-require-project-root nil)
  (setq projectile-project-root-files 
        (append '(".svn") projectile-project-root-files))
  (setq projectile-globally-ignored-directories
        (append '(".svn") projectile-globally-ignored-directories))

  ; Define ibuffer filter groups for each known project
  (defun my/define-projectile-filter-groups ()
    (when (boundp 'projectile-known-projects)
      (setq my/project-filter-groups
            (mapcar
             (lambda (it)
               (let ((name (file-name-nondirectory (directory-file-name it))))
                 `(,name (filename . ,(expand-file-name it)))))
             projectile-known-projects))))

  ;; Set up default ibuffer filter groups
  (setq ibuffer-saved-filter-groups
        (list
         (cons "default"
               (append
                (my/define-projectile-filter-groups)
                ;; ... whatever other groups you want, e.g.
                '(("elisp" (mode . emacs-lisp-mode))
                  ("C&C++" (or (mode . c-mode)
                               (mode . c++-mode)))
                  ("emacs" (or
                            (name . "^\\*scratch\\*$")
                            (name . "^\\*Messages\\*$")
                            (mode . eshell-mode)))
                  ("gnus" (or
                           (mode . message-mode)
                           (mode . bbdb-mode)
                           (mode . mail-mode)
                           (mode . gnus-group-mode)
                           (mode . gnus-summary-mode)
                           (mode . gnus-article-mode)
                           (name . "^\\.bbdb$")
                           (name . "^\\.newsrc-dribble")))))))))

;; Enable default groups by default
(add-hook 'ibuffer-mode-hook
              (lambda ()
                (ibuffer-switch-to-saved-filter-groups "default")))

;; You probably don't want to see empty project groups
(setq ibuffer-show-empty-filter-groups nil)



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
  ;; Plato Wu,2013/07/24: use this to random play
  (emms-player-mpd-send  "random 1" nil #'ignore)
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
  (defun emms-pause-after-10mintue ()
    (interactive)
    (sleep-for 600)
    (emms-pause))
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
  ;; (emms-history-load)
  ;; Plato Wu,2014/04/23: 
  (defun emms-history-load ()
    "restore `emms-history-file', if it is empty, then load `emms-history-file'bak."
    (interactive)
    (when (and (stringp emms-history-file)
             (file-exists-p emms-history-file))
    (let (history buf)
      (with-temp-buffer
        (emms-insert-file-contents emms-history-file)
        (setq history (read (current-buffer)))
        (setq playlists (cadr history))
        (unless playlists 
          (erase-buffer)
          (emms-insert-file-contents (concat emms-history-file ".bak"))
          (setq history (read (current-buffer))))
        (dolist (playlist (cadr history))
          (with-current-buffer (emms-playlist-new (car playlist))
            (setq emms-playlist-buffer (current-buffer))
            (if (string= (car playlist) (car history))
                (setq buf (current-buffer)))
            (mapc 'emms-playlist-insert-track
                  (nth 2 playlist))
            (ignore-errors
              (emms-playlist-select (cadr playlist)))))
        (setq emms-playlist-buffer buf)
        (dolist (method (nth 2 history))
          (set (car method) (cdr method)))
        (ignore-errors
          (emms-start))))))
  (setq emms-lyrics-dir "~/Music/Lyrics")
  (setq emms-lyrics-coding-system 'gbk-dos)
                                        ;    (setq emms-lyrics-display-on-minibuffer t)
;  (emms-lyrics 1)
  (defadvice gnus-group-get-new-news (around pause-emms activate)
    "Pause emms while Gnus is fetching mails or news."
    (if emms-player-playing-p
        (progn (emms-pause)
               ad-do-it
               (emms-pause))
      ad-do-it))
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

(defadvice emms-history-save (before emm-history-save-set-repeat-flag activate)
  "set emms-repeat-track nil for mpd which restart without remember repeating"
  (setq emms-repeat-track nil))

  ;; Plato Wu,2013/07/23: let emms-player-mpd support single track-repeat.
(defun emms-mpd-toggle-repeat-track ()
  "Toggle whether emms repeats the current track.
See  `emms-repeat-track'."
  (interactive)
  (setq emms-repeat-track (not emms-repeat-track))
  (if emms-repeat-track
      (progn
        (emms-player-mpd-send  "repeat 1" nil #'ignore)
        ;; Plato Wu,2013/07/25: single mode is introduced with MPD 0.15
        (emms-player-mpd-send  "single 1" nil #'ignore)
        (message "Will repeat the current track."))
    (emms-player-mpd-send  "repeat 0" nil #'ignore)
    (emms-player-mpd-send  "single 0" nil #'ignore)
    (message "Will advance to the next track after this one."))))

(defvar my-authinfo "~/.authinfo")

(defun auctex-configuration ()
;  (require 'preview-latex)
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
;(auctex-configuration)

(defun blogger-configuration ()
;; Plato Wu,2011/01/23: it is need by blogger-configuration and org-toodledo-configuration
  (require 'netrc)
  (require 'muse) 
  (require 'muse-mode) 
  (require 'muse-publish) 
  (require 'muse-html)
  (require 'weblogger)
  (require 'muse-latex2png)
  (require 'muse-colors)
  (require 'w3m)
;; I also want to use regexp to markup inline latex equations of the
;; form `$\alpha$' because I'm too lazy to write
;; <latex inline="t">$\alpha$</latex>
  (add-to-list 'muse-html-markup-regexps
               '(1600 "\\$[^$]*\\$" 0 kid-muse-html-latex-inline))

  
  ;(setq muse-html-markup-regexps '((10000 "\\(\\(\n\\(?:[[:blank:]]*\n\\)*\\([[:blank:]]*\n\\)\\)\\|\\`\\s-*\\|\\s-*\\'\\)" 3 muse-html-markup-paragraph)))
  (defun kid-muse-html-latex-inline ()
    (let ((attrs `(("inline" . "true"))))
      (muse-publish-latex-tag (match-beginning 0) (match-end 0) attrs)
      ;; Plato Wu,2013/01/03: return string will cause error
      nil))
  (setq muse-latex2png-scale-factor 1.5)
  (add-to-list 'muse-colors-tags
             '("latex" t t nil muse-colors-example-tag))
  ;; Plato Wu,2013/01/03: in order to user internet path for latex image, modify this variable
  ;; and muse-latex2png-region & muse-latex2png-move2pubdir.
  ;; (setq muse-latex2png-img-dest "http://plato.ninth.su/Photos/%E6%B7%B1%E5%85%A5%E6%B5%85%E5%87%BA%E8%BF%90%E5%8A%A8%E6%8E%A7%E5%88%B6")

  (add-to-list 'auto-mode-alist '("\\.muse$" . muse-mode))
  (define-key muse-mode-map "u" 'muse-insert-url)
  (define-key muse-mode-map "l"
    #'(lambda ()
       (interactive)
       (insert "<literal style=\"blogger\">\n\n</img>\n</a>\n</literal>")
       (previous-line 3)))
  (setq muse-insert-url-initial-input nil)
  (add-hook 'muse-mode-hook 
            #'(lambda ()
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
          (rule . "------华------丽------的------分------割------线------")
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
                        (cdr (assoc "password" netrc-data)) "1"))))
    (weblogger-select-configuration))

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
      ;; Plato Wu,2013/01/02: use digest in mail,  don't use duoshuo.
      ;; Plato Wu,2013/01/02: SMTP will report error, but it is OK, maybe the port is change
      ;; from 25 to 465
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
                             "\n")))
         (setq user-mail-address "plato.wu@qq.com")
         ;; (setq user-mail-address "68697211@qq.com")
         (setq smtpmail-default-smtp-server "smtp.qq.com") 
         (setq smtpmail-smtp-server "smtp.qq.com") 
         (setq send-mail-function 'smtpmail-send-it)
         ;; Plato Wu,2014/01/25: it seems these variables is obselete
         ;; (setq smtpmail-starttls-credentials 
         ;;       '(("smtp.qq.com" 25 nil nil)))
         ;; (setq smtpmail-auth-credentials )
         ;; Plato Wu,2009/12/19: QQ smtp server is garbage!!!
         ;; It can not used in post2qzone for Wordpress, it said content is reject
         ;; It can not used DJ EmailPublish for wordpress
         ;; It need set smtpmail-starttls-credentials as 25 but not 465 in Gnus.
         ;; Plato Wu,2014/01/25: now smtp.qq.com must use tls, but don't work for
         ;; set smtpmail-stream-type as nil()
         (setq smtpmail-smtp-service 465)
         (setq smtpmail-stream-type 'tls)
;         (setq smtpmail-stream-type 'ssl)

         ;; (setq smtpmail-debug-info t)
         ;; (setq smtpmail-debug-verb t)
         (setq smtpmail-smtp-user "42662703")
         ;; Plato Wu,2010/09/28: delete weiruan000.platowu@spaces.live.com, Live Space will be shutdown
         ;; use its connect RSS instead.
         (compose-mail "42662703@qzone.qq.com"
                       ;;68697211@qzone.qq.com,
                       (cdr (assoc "title" entry)))
         (let ((content (cdr (assoc "content" entry))))
           ;; (insert (substring content 0 
           ;;                    (+ 4 (string-match "</p>" content))))
           (insert (format "<p>....</p><p>请移步<a href=\"http://platowu.info\">恒永之地</a>查看<a href=\"http://platowu.info/%s\"><font size = 6 color=\"#FF0000\">点我</font></a>，评论也在那边留哦，:)</p>" 
                           (w3m-url-encode-string
                            (cdr (assoc "title" entry)) 'utf-8))))
         ;; (mail-send-and-exit)
         (message-send-and-exit)))))
;; Plato Wu,2013/01/02: refine to support multi footnote for many article in one page.
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
  ;; Plato Wu,2013/08/24: emacs 24 use message-send-and-exit instead mail-send-and-exit
;;  (require 'smtpmail)
  ;; (defun smtpmail-send-it ()
  ;;   (let ((errbuf (if mail-interactive
  ;;                     (generate-new-buffer " smtpmail errors")
  ;;                   0))
  ;;         (tembuf (generate-new-buffer " smtpmail temp"))
  ;;         (case-fold-search nil)
  ;;         delimline
  ;;         (mailbuf (current-buffer))
  ;;         ;; Examine this variable now, so that
  ;;         ;; local binding in the mail buffer will take effect.
  ;;         (smtpmail-mail-address
  ;;          (or (and mail-specify-envelope-from (mail-envelope-from))
  ;;              user-mail-address))
  ;;         (smtpmail-code-conv-from
  ;;          (if enable-multibyte-characters
  ;;              (let ((sendmail-coding-system smtpmail-code-conv-from))
  ;;                (select-message-coding-system)))))
  ;;     (unwind-protect
  ;;         (save-excursion
  ;;           (set-buffer tembuf)
  ;;           (erase-buffer)
  ;;           ;; Use the same `buffer-file-coding-system' as in the mail
  ;;           ;; buffer, otherwise any `write-region' invocations (e.g., in
  ;;           ;; mail-do-fcc below) will annoy with asking for a suitable
  ;;           ;; encoding.
  ;;           (set-buffer-file-coding-system smtpmail-code-conv-from nil t)
  ;;           (insert-buffer-substring mailbuf)
  ;;           (goto-char (point-max))
  ;;           ;; require one newline at the end.
  ;;           (or (= (preceding-char) ?\n)
  ;;               (insert ?\n))
  ;;           ;; Change header-delimiter to be what sendmail expects.
  ;;           (mail-sendmail-undelimit-header)
  ;;           (setq delimline (point-marker))
  ;;           ;; (sendmail-synch-aliases)
  ;;           (if mail-aliases
  ;;               (expand-mail-aliases (point-min) delimline))
  ;;           (goto-char (point-min))
  ;;           ;; ignore any blank lines in the header
  ;;           (while (and (re-search-forward "\n\n\n*" delimline t)
  ;;                       (< (point) delimline))
  ;;             (replace-match "\n"))
  ;;           (let ((case-fold-search t))
  ;;             ;; We used to process Resent-... headers here,
  ;;             ;; but it was not done properly, and the job
  ;;             ;; is done correctly in `smtpmail-deduce-address-list'.
  ;;             ;; Don't send out a blank subject line
  ;;             (goto-char (point-min))
  ;;             (if (re-search-forward "^Subject:\\([ \t]*\n\\)+\\b" delimline t)
  ;;                 (replace-match "")
  ;;               ;; This one matches a Subject just before the header delimiter.
  ;;               (if (and (re-search-forward "^Subject:\\([ \t]*\n\\)+" delimline t)
  ;;                        (= (match-end 0) delimline))
  ;;                   (replace-match "")))
  ;;             ;; Put the "From:" field in unless for some odd reason
  ;;             ;; they put one in themselves.
  ;;             (goto-char (point-min))
  ;;             (if (not (re-search-forward "^From:" delimline t))
  ;;                 (let* ((login smtpmail-mail-address)
  ;;                        (fullname (user-full-name)))
  ;;                   (cond ((eq mail-from-style 'angles)
  ;;                          (insert "From: " fullname)
  ;;                          (let ((fullname-start (+ (point-min) 6))
  ;;                                (fullname-end (point-marker)))
  ;;                            (goto-char fullname-start)
  ;;                            ;; Look for a character that cannot appear unquoted
  ;;                            ;; according to RFC 822.
  ;;                            (if (re-search-forward "[^- !#-'*+/-9=?A-Z^-~]"
  ;;                                                   fullname-end 1)
  ;;                                (progn
  ;;                                  ;; Quote fullname, escaping specials.
  ;;                                  (goto-char fullname-start)
  ;;                                  (insert "\"")
  ;;                                  (while (re-search-forward "[\"\\]"
  ;;                                                            fullname-end 1)
  ;;                                    (replace-match "\\\\\\&" t))
  ;;                                  (insert "\""))))
  ;;                          (insert " <" login ">\n"))
  ;;                         ((eq mail-from-style 'parens)
  ;;                          (insert "From: " login " (")
  ;;                          (let ((fullname-start (point)))
  ;;                            (insert fullname)
  ;;                            (let ((fullname-end (point-marker)))
  ;;                              (goto-char fullname-start)
  ;;                              ;; RFC 822 says \ and nonmatching parentheses
  ;;                              ;; must be escaped in comments.
  ;;                              ;; Escape every instance of ()\ ...
  ;;                              (while (re-search-forward "[()\\]" fullname-end 1)
  ;;                                (replace-match "\\\\\\&" t))
  ;;                              ;; ... then undo escaping of matching parentheses,
  ;;                              ;; including matching nested parentheses.
  ;;                              (goto-char fullname-start)
  ;;                              (while (re-search-forward
  ;;                                      "\\(\\=\\|[^\\]\\(\\\\\\\\\\)*\\)\\\\(\\(\\([^\\]\\|\\\\\\\\\\)*\\)\\\\)"
  ;;                                      fullname-end 1)
  ;;                                (replace-match "\\1(\\3)" t)
  ;;                                (goto-char fullname-start))))
  ;;                          (insert ")\n"))
  ;;                         ((null mail-from-style)
  ;;                          (insert "From: " login "\n")))))
  ;;             ;; Insert a `Message-Id:' field if there isn't one yet.
  ;;             (goto-char (point-min))
  ;;             (unless (re-search-forward "^Message-Id:" delimline t)
  ;;               (insert "Message-Id: " (message-make-message-id) "\n"))
  ;;             ;; Insert a `Date:' field if there isn't one yet.
  ;;             (goto-char (point-min))
  ;;             (unless (re-search-forward "^Date:" delimline t)
  ;;               (insert "Date: " (message-make-date) "\n"))
  ;;             ;; Possibly add a MIME header for the current coding system
  ;;             (let (charset)
  ;;               (goto-char (point-min))
  ;;               (and (eq mail-send-nonascii 'mime)
  ;;                    (not (re-search-forward "^MIME-version:" delimline t))
  ;;                    (progn (skip-chars-forward "\0-\177")
  ;;                           (/= (point) (point-max)))
  ;;                    smtpmail-code-conv-from
  ;;                    (setq charset
  ;;                          (coding-system-get smtpmail-code-conv-from
  ;;                                             'mime-charset))
  ;;                    (goto-char delimline)
  ;;                    (insert "MIME-version: 1.0\n"
  ;;                            ;; Plato Wu,2009/12/19: use html format
  ;;                            "Content-type: text/html; charset="
  ;;                            (symbol-name charset)
  ;;                            "\nContent-Transfer-Encoding: 8bit\n")))
  ;;             ;; Insert an extra newline if we need it to work around
  ;;             ;; Sun's bug that swallows newlines.
  ;;             (goto-char (1+ delimline))
  ;;             (if (eval mail-mailer-swallows-blank-line)
  ;;                 (newline))
  ;;             ;; Find and handle any FCC fields.
  ;;             (goto-char (point-min))
  ;;             (if (re-search-forward "^FCC:" delimline t)
  ;;                 ;; Force `mail-do-fcc' to use the encoding of the mail
  ;;                 ;; buffer to encode outgoing messages on FCC files.
  ;;                 (let ((coding-system-for-write smtpmail-code-conv-from))
  ;;                   (mail-do-fcc delimline)))
  ;;             (if mail-interactive
  ;;                 (with-current-buffer errbuf
  ;;                   (erase-buffer))))
  ;;           ;;
  ;;           (setq smtpmail-address-buffer (generate-new-buffer "*smtp-mail*"))
  ;;           (setq smtpmail-recipient-address-list
  ;;                 (smtpmail-deduce-address-list tembuf (point-min) delimline))
  ;;           (kill-buffer smtpmail-address-buffer)
  ;;           (smtpmail-do-bcc delimline)
  ;;           ;; Send or queue
  ;;           (if (not smtpmail-queue-mail)
  ;;               (if (not (null smtpmail-recipient-address-list))
  ;;                   (if (not (smtpmail-via-smtp
  ;;                             smtpmail-recipient-address-list tembuf))
  ;;                       (error "Sending failed; SMTP protocol error"))
  ;;                 (error "Sending failed; no recipients"))
  ;;             (let* ((file-data
  ;;                     (expand-file-name
  ;;                      (format "%s_%i"
  ;;                              (format-time-string "%Y-%m-%d_%H:%M:%S")
  ;;                              (setq smtpmail-queue-counter
  ;;                                    (1+ smtpmail-queue-counter)))
  ;;                      smtpmail-queue-dir))
  ;;                    (file-data (convert-standard-filename file-data))
  ;;                    (file-elisp (concat file-data ".el"))
  ;;                    (buffer-data (create-file-buffer file-data))
  ;;                    (buffer-elisp (create-file-buffer file-elisp))
  ;;                    (buffer-scratch "*queue-mail*"))
  ;;               (unless (file-exists-p smtpmail-queue-dir)
  ;;                 (make-directory smtpmail-queue-dir t))
  ;;               (with-current-buffer buffer-data
  ;;                 (erase-buffer)
  ;;                 (set-buffer-file-coding-system smtpmail-code-conv-from nil t)
  ;;                 (insert-buffer-substring tembuf)
  ;;                 (write-file file-data)
  ;;                 (set-buffer buffer-elisp)
  ;;                 (erase-buffer)
  ;;                 (insert (concat
  ;;                          "(setq smtpmail-recipient-address-list '"
  ;;                          (prin1-to-string smtpmail-recipient-address-list)
  ;;                          ")\n"))
  ;;                 (write-file file-elisp)
  ;;                 (set-buffer (generate-new-buffer buffer-scratch))
  ;;                 (insert (concat file-data "\n"))
  ;;                 (append-to-file (point-min)
  ;;                                 (point-max)
  ;;                                 (expand-file-name smtpmail-queue-index-file
  ;;                                                   smtpmail-queue-dir)))
  ;;               (kill-buffer buffer-scratch)
  ;;               (kill-buffer buffer-data)
  ;;               (kill-buffer buffer-elisp))))
  ;;       (kill-buffer tembuf)
  ;;       (if (bufferp errbuf)
  ;;           (kill-buffer errbuf)))))
  )

;; (defun smart-tab-configuration ()
;; ;; Plato Wu,2014/01/10: dabbrev-expand is better than hippie expand when coding
;; ;;  (setq smart-tab-using-hippie-expand t)
;;   (global-smart-tab-mode 1))

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

(defun org-mode-configuration ()
  ;;Plato Wu,2015/06/05: for org package, when installing from ELPA, please do so from a fresh Emacs
  ;;session where no org function has been called, use list-packages and press d & x to delete old version
  ;; and i & x to install new one.
  ;; Plato Wu,2010/08/29: use C-u C-c $ org-archive-subtree to archive DONE items
  ;; (require 'org-archive)
  ;; (require 'org-publish)
  (require 'org)
  ;; Plato Wu,2013/07/15: cygwin need (prefer-coding-system 'utf-8) and filename is english to export Chinese pdf
  ;; Plato Wu,2014/01/23: use (org-beamer-export-to-pdf) to export presentation, the presentation filename must be English in Cygwin
  (if (string<  "8.0" org-version)
    (progn
      (require 'org-crypt)
      (require 'org-table)
      (require 'ox-beamer)
      ;; Plato Wu,2011/04/22: use xelatex to do better with Chinese, and use system font.
      ;; Plato Wu,2013/07/10: @todo maybe use org-latex-text-markup-alist instead org-export-latex-emphasis-alist
      (setq org-latex-pdf-process '(" [ ! -d log/ ] && mkdir log || echo 0"
                                   "xelatex -output-directory  log/ %f" 
                                   ;; moving intermediate tex file
                                   "mv `basename %b`.tex log/"
                                   ;; moving pdf for meeting org-latex-export-to-pdf
                                   "mv log/`basename %b`.pdf ."))
      (add-to-list 'org-latex-classes
             '("CV"
               "\\documentclass{CV}
                \\title{}"
               ("\\cvsection{%s}" . "\\cvsection{%s}")
               ("\\subsection{%s}" . "\\subsection{%s}"))))
    (require 'org-crypt)
    (require 'org-latex)
;Plato Wu,2013/07/15: org-verison < 8.0 don't support BEAMER_THEME and ATTR_BEAMER
;    (require 'org-beamer)
    (setq org-latex-to-pdf-process '(" [ ! -d log/ ] && mkdir log || echo 0"
                                   "xelatex -output-directory  log/ %f" 
                                   ;; moving intermediate tex file
                                   "mv `basename %b`.tex log/"
                                   ;; moving pdf for meeting org-latex-export-to-pdf
                                   "mv log/`basename %b`.pdf ."))
  ;; Plato Wu,2011/02/17: protected all emphasis text for there is a bug
  ;; for text which contains number.
    (add-to-list 'org-export-latex-emphasis-alist
                ;; Plato Wu,2011/02/18: use ~ to tag Chinese characters for song font
                ;; if we use font as main font, the english font is ugly.
                ;; Plato Wu,2012/08/28: org-emph-re only support "[*/_=~+]"
                '("~" "\\kai{%s}" t ))
    (add-to-list 'org-export-latex-classes
             '("CV"
               "\\documentclass{CV}
                \\title{}"
               ("\\cvsection{%s}" . "\\cvsection{%s}")
               ("\\subsection{%s}" . "\\subsection{%s}"))))
  

  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  ;; Plato Wu,2013/07/24: auto fill lone line
  (setq org-startup-truncated nil)
  ;; Turn off prefer future dates 
  (setq org-read-date-prefer-future nil)

  (setq org-list-demote-modify-bullet (quote (("+" . "-")
                                              ("*" . "-")
                                              ("1." . "-")
                                              ("1)" . "-"))))
                                        ;disable priority commands.
  (setq org-enable-priority-commands nil)

  (define-key org-mode-map (kbd "C-M-j") 'org-insert-todo-heading)

  (setq org-modules 
        '(org-bbdb org-bibtex org-crypt org-gnus org-info
                   org-jsinfo org-inlinetask org-irc org-mew org-mhe org-vm org-wl org-w3m))

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

  ;; Plato Wu,2011/02/18: use org-latex-export-to-pdf instead
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
  
  (setq org-export-copy-to-kill-ring nil)
  (setq org-confirm-babel-evaluate nil)
  ;; Plato Wu,2015/11/25: not using org-babel now
  ;; (org-babel-do-load-languages
  ;;  'org-babel-load-languages
  ;;  '((R . t)
  ;;    (ditaa . t)
  ;;    (dot . t)
  ;;    (emacs-lisp . t)
  ;;    (gnuplot . t)
  ;;    (python . t)
  ;;    (perl . t)
  ;;    (sh . t)
  ;;    (C . t)
  ;;    (sqlite . t)))  

  (defun orgtbl-to-latex (table params)
  "Convert the orgtbl-mode TABLE to LaTeX.
TABLE is a list, each entry either the symbol `hline' for a horizontal
separator line, or a list of fields for that line.
PARAMS is a property list of parameters that can influence the conversion.
Supports all parameters from `orgtbl-to-generic'.  Most important for
LaTeX are:

:splice    When set to t, return only table body lines, don't wrap
           them into a tabular environment.  Default is nil.

:fmt       A format to be used to wrap the field, should contain %s for the
           original field value.  For example, to wrap everything in dollars,
           use :fmt \"$%s$\".  This may also be a property list with column
           numbers and formats.  For example :fmt (2 \"$%s$\" 4 \"%s%%\")
           The format may also be a function that formats its one argument.

:efmt      Format for transforming numbers with exponentials.  The format
           should have %s twice for inserting mantissa and exponent, for
           example \"%s\\\\times10^{%s}\".  LaTeX default is \"%s\\\\,(%s)\".
           This may also be a property list with column numbers and formats.
           The format may also be a function that formats its two arguments.

:llend     If you find too much space below the last line of a table,
           pass a value of \"\" for :llend to suppress the final \\\\.

The general parameters :skip and :skipcols have already been applied when
this function is called."
  (let* ((alignment (mapconcat #'(lambda (x) (if x "r" "l"))
			       org-table-last-alignment ""))
	 (params2
	  (list
           ;; Plato Wu,2013/08/28: remove aligment command
;	   :tstart (concat "\\begin{tabular}{" alignment "}")
           :tstart "\\begin{tabular}"
	   :tend "\\end{tabular}"
	   :lstart "" :lend " \\tabularnewline" :sep " & "
	   :efmt "%s\\,(%s)" :hline "\\hline")))
    (orgtbl-to-generic table (org-combine-plists params2 params))))
  )

(defun google-c-style-configuration ()
;  (require 'google-c-style)
;  (autoload 'google-set-c-style "google-c-style" nil t)
  (add-hook 'c-mode-common-hook 
            #'(lambda () 
                (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
                 ;; (c-set-style "stroustrup")
                 ;; (c-toggle-auto-hungry-state nil)
                  (google-set-c-style)
                  ;; Plato Wu,2014/06/26: follow visual studio style
                  (setq c-basic-offset 4)
                  ;; The start of a C preprocessor macro definition.
                  (c-set-offset 'cpp-macro 0)))))
              

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
;;      #'(lambda ()
;; 	(sawfish-eval "(quit)"))
;;      t)))
;; (define-key scheme-mode-map [f9] 'gds-show-last-stack)

;; (defun ggtags-configuration ()
;; ;; Plato Wu,2014/01/10: M-. finds definitions or references according to the tag at point, 
;; ;; if point is at a definition tag find references and vice versa. M-] finds references.
;; ;; If multiple matches are found, navigation mode is entered, the mode-line lighter 
;; ;; changed, and a navigation menu-bar entry presented. In this mode, M-n and M-p moves 
;; ;; to next and previous match, M-} and M-{ to next and previous file respectively. M-o 
;; ;; toggles between full and abbreviated displays of file names in the auxiliary popup 
;; ;; window. When you locate the right match, press RET to finish which hides the 
;; ;; auxiliary window and exits navigation mode. You can continue the search using M-,. 
;; ;; To abort the search press M-*.

;; ;; Normally after a few searches a dozen buffers are created visiting files tracked 
;; ;; by GNU Global. C-c M-k helps clean them up.
;;   (add-hook 'c-mode-common-hook
;;           (lambda ()
;;             (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
;;               (ggtags-mode 1)))))

(defun smart-compile-configuration ()
;  (autoload 'smart-compile "smart-compile" "load smart compile")
;Plato Wu,2013/04/15: use (symbol-function 'cc-mode) to get autoload for eval-after-load
;Plato Wu,2013/05/14: there is cc-mode.el) to get autoload for eval-after-load
 (eval-after-load
    "cc-mode"
   '(progn (define-key c-mode-map [f7] 'smart-compile)
          (define-key c++-mode-map [f7] 'smart-compile)))

  (eval-after-load
    "make-mode"
    '(define-key makefile-mode-map [f7] 'smart-compile))

  (setq compilation-read-command nil)
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

;(compile-configuration)

(defun common-lisp-configuration ()
  (cldoc-configuration)
;; Plato Wu,2013/06/06: (ql:update-client) and (ql:update-all-dists) 
  (autoload 'slime "~/quicklisp/slime-helper.el" "The Superior Lisp Interaction Mode for Emacs" t nil)
;; slime-inspector-mode's quick key
;; l runs the command slime-inspector-pop, return top level
;; d runs the command slime-inspector-describe
;; sldb-mode's quick key
;; v runs the command sldb-show-source
;; swank::inspect-function can describe a function
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
  (add-hook 'lisp-mode-hook #'(lambda () (slime-mode t)))
  ;; Plato Wu,2009/12/12: provisional clear lisp connection closed unexpectedly
  ;; problem
  ;; Plato Wu,2012/09/15: it will cause problem for latest sbcl & slime
  ;; (defun load-swank-dont-close (port-filename encoding)
  ;;   (format "%S\n\n"
  ;;           `(progn
  ;;              (load ,(expand-file-name slime-backend slime-path) :verbose t)
  ;;              (funcall (read-from-string "swank-loader:init"))
  ;;              (funcall (read-from-string "swank:start-server")
  ;;                       ,port-filename
  ;;                       :coding-system ,(slime-coding-system-cl-name encoding)
  ;;                       :dont-close t))))
  ;; (setq slime-lisp-implementations
  ;;       '((sbcl-noclose ("sbcl" "-quiet") :init load-swank-dont-close)))
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
  (mapc #'(lambda (mode) 
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
  ;; Plato Wu,2013/04/15: TODO to check lisp-mode is OK by symbol-function
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
     #'(lambda ()
       (let ((distance (skip-syntax-forward "w_")))
         (when (zerop distance) (forward-char 1))
         (point)))))

  (add-hook 'lisp-mode-hook
            #'(lambda ()
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

(eval-after-load
   "~/quicklisp/slime-helper.el"
   '(common-lisp-configuration))

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
  ;; Plato Wu,2013/06/19: is it obsolete in w3m/0.5.3
  ;; (add-hook 'w3m-mode-hook 
  ;;            #'(lambda ()
  ;;               (w3m-link-numbering-mode 1)
  ;;               ))
  
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

;; (defun ascii-configuration ()
;;   (autoload 'ascii-on "ascii" "Turn on ASCII code display." t))

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

(defun cedet-configuration ()
  ;; (when
  ;;     (try-load-file
  ;;      "~/.emacs.d/el-get/cedet/cedet-devel-load.el")
                                        ; cogre-uml-quick-class for create uml diagram
    (global-ede-mode 1)        ; Enable the Project management system
    (global-srecode-minor-mode 1)
    (semantic-load-enable-code-helpers)      ; Enable prototype help and smart completion
    ;;(semantic-load-enable-gaudy-code-helpers)  ;ng for no menu emacs
    ;;(semantic-load-enable-excessive-code-helpers); ng for no menu emacs
    ;;(semantic-load-enable-semantic-debugging-helpers) ;for debug
    ;;(require 'semantic/sb)
    ;; )
  )

(defun auto-complete-configuration ()
  ;; Plato Wu,2016/11/04: need require auto-complete-config,
  ;; require auto-complete is not enough
  (require 'auto-complete-config)
  (ac-config-default)
  (setq ac-auto-start 3)
  (add-hook 'c-mode-common-hook 
            (lambda ()
              (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
;;                 (require 'semantic/ia)
;;                 (require 'semantic/mru-bookmark)
;;                 (semantic-mode 1)
;;                 (add-to-list 'semantic-default-submodes 'global-semantic-mru-bookmark-mode)
;;                 (defun semantic-ia-find-tag (point)
;;                   "Try semantic-ia-fast-jump first, then try find-tag"
;;                   (interactive "d")
;; ;;                  (semantic-ia-fast-jump point)
;;                   (condition-case nil
;;                       (semantic-ia-fast-jump point)
;;                     (error
;;                      (vj-find-tag)))
;;                   )
;;                 (setq semantic-symref-auto-expand-results t)
;;                 (defun semantic-ia-find-reference (regexp)
;;                   (interactive
;;                    (let ((regexp (grep-read-regexp)))
;;                      (list regexp)))
;;                   (semantic-symref-symbol regexp))
;; ;;                (define-key  (keymap-symbol (current-local-map)) "\M-]"  )
;;                 (define-key (current-local-map) "\M-." 'semantic-ia-find-tag)
;;                 (define-key (current-local-map) "\M-]" 'semantic-ia-find-reference)
;;                 ;; Plato Wu,2014/03/21: after 23.3, there is no push-tag-mark in etags.el
;;                 ;; but semantic-ia-fast-jump need it.
;;                 (unless (fboundp 'push-tag-mark)
;;                   (defun push-tag-mark ()
;;                     "Push the current position to the ring of markers so that
;;                    \\[pop-tag-mark] can be used to come back to current position."
;;                     (interactive)
;;                     (ring-insert find-tag-marker-ring (point-marker))))

               ;; Plato Wu,2014/03/14: no pulse highlight
               (setq pulse-flag 'never)
               ;; Plato Wu,2014/03/26: backspace don't invoke auto-complete
               (add-to-list 'ac-non-trigger-commands 'c-electric-backspace)
;;                ;; Plato Wu,2014/03/14: there is not this device in buildin CEDET 
;;                (defadvice push-mark (around semantic-mru-bookmark activate)
;;                  "Push a mark at LOCATION with NOMSG and ACTIVATE passed to `push-mark'.
;; If `semantic-mru-bookmark-mode' is active, also push a tag onto
;; the mru bookmark stack."
;;                  (semantic-mrub-push semantic-mru-bookmark-ring
;;                                      (point)
;;                                      'mark)
;;                  ad-do-it)
;;                (defun semantic-ia-fast-jump-back ()
;;                  (interactive)
;;                  (if (ring-empty-p (oref semantic-mru-bookmark-ring ring))
;;                      (error "Semantic Bookmark ring is currently empty"))
;;                  (let* ((ring (oref semantic-mru-bookmark-ring ring))
;;                         (alist (semantic-mrub-ring-to-assoc-list ring))
;;                         (first (cdr (car alist))))
;;                    (if (semantic-equivalent-tag-p (oref first tag) (semantic-current-tag))
;;                        (setq first (cdr (car (cdr alist)))))
;;                    (semantic-mrub-switch-tags first)))
;;                (define-key (current-local-map) "\M-*" 'semantic-ia-fast-jump-back)

                (add-to-list 'ac-sources 'ac-source-semantic)))))

(eval-after-load 'autoinsert
  '(when (is-version 24) 
     ;; Plato Wu,2014/08/26: define-auto-insert can't delete ("\\.\\([Hh]\\|hh\\|hpp\\)\\'" . "C / C++ header") case
     (setq auto-insert-alist
           (cl-delete-if #'(lambda (elt) (equal (car elt) '("\\.\\([Hh]\\|hh\\|hpp\\)\\'" . "C / C++ header"))) 
                         auto-insert-alist))
     (setq auto-insert-alist
           (cl-delete-if #'(lambda (elt) (equal (car elt) '("\\.\\([Cc]\\|cc\\|cpp\\)\\'" . "C / C++ program"))) 
                         auto-insert-alist))
     
     ;; Plato Wu,2014/08/26: n means indent after break-line, "\n" don't do it, refer to skeleton-insert
     ;; _ means cursor's location.
     (define-auto-insert '("\\.\\([Hh]\\|hh\\|hpp\\)\\'" . "C / C++ header")
       '((upcase
          (concat
           (file-name-nondirectory
            (file-name-sans-extension buffer-file-name))
           "_"
           (file-name-extension buffer-file-name)))
         "#ifndef " str n "#define " str n n
         (if (string= "hpp" (file-name-extension buffer-file-name)) 
             '_
           "#ifdef __cplusplus")
         & n & "extern \"C\" {\n" & "#endif" & n & n & _ 
         (unless (string= "hpp" (file-name-extension buffer-file-name)) 
             'n
           ) & "\n#ifdef __cplusplus\n" & "}" & n  & "#endif"
         "\n\n#endif\n"))

     (define-auto-insert '("\\.\\([Cc]\\|cc\\|cpp\\)\\'" . "C / C++ program")
      '(
        "Short description: "
        "/**\n * "
        (file-name-nondirectory (buffer-file-name))
        " -- " str \n
        " *" \n
        " * Written by " (user-full-name) " on " (let ((system-time-locale "en_US.utf8"))
                           (format-time-string "%Y-%m-%d %A")) \n
        " */" > \n \n
        "#include \""
        (file-name-sans-extension
         (file-name-nondirectory (buffer-file-name)))
        (if (string= "cpp" (file-name-extension buffer-file-name))
         ".hpp\""   
         ".h\"") \n \n))
     ))

(defun webblogger-configuration ()
 (eval-after-load "muse-mode" '(blogger-configuration)))

(defun xclip-configuration ()
  (when (and (getenv "DISPLAY")
             (not (is-system "windows-nt")))
    ;(autoload 'turn-on-xclip "xclip" "exchange clip between X and emacs" t nil)
    (turn-on-xclip)))

;; 'mode-local-init-hook
;; (autoloadp (symbol-function 'ascii-on))

(provide 'my-packages)
