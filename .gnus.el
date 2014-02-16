;; Plato Wu,2010/04/08: Q: Meet error:  "gnus-agent-read-agentview no longer supports version 1.  Stop gnus, manually evaluate gnus-agent-convert-to-compressed-agentview, then restart gnus."
;;A: Open legacy-gnus-agent.el and M-x eval-current-buffer and evaluate:
;;(gnus-agent-convert-to-compressed-agentview ".newsrc"). It is OK now!

(defun harald-send-nntp-authinfo ()
  (let ((netrc-data 
         (netrc-machine 
          (netrc-parse my-authinfo) 
          nntp-address "80")))
    ;; Plato Wu,2010/09/01: if nntp-address does not require login then ignore it.
    (if netrc-data
        (let ((login (cdr (assoc "login" netrc-data)))
              (passwd (cdr (assoc "password" netrc-data))))
          (nntp-send-command "^.*\r?\n" "AUTHINFO USER" login)
          (nntp-send-command "^.*\r?\n" "AUTHINFO PASS" passwd)))))

(add-hook 'nntp-server-opened-hook 'harald-send-nntp-authinfo)

(setq gnus-select-method 
      ;; standin000,2010/08/10: re-register in eternal-september.org
      ;; and now 80 and 443 both are available, use 80
      '(nntp "reader80.eternal-september.org" (nntp-port-number 80))
;      '(nntp "reader443.eternal-september.org" (nntp-port-number 443))
      )

;enable the mode for some server may be not accessed in somewhere.
(setq gnus-batch-mode t)

(if (not (file-readable-p "~/limited_network"))
    (progn
      (setq gnus-secondary-select-methods 
	    '(;; (nnfolder "") ;you want to read your mail with the nnfolder back end,
              ;; ;set how to store email
              ;; ;(setq pop3-leave-mail-on-server t)
              ;; Plato Wu,2011/10/29: it is broken.
;              (nntp "news.cn99.com")
              ;(nntp "news.yaako.com")
	      (nntp "news.newsfan.net")
	      (nntp "news.gmane.org")
              ;; Plato Wu,2011/05/31: B-Del don't work in virtual folder which contain
              ;; Inbox & Sent Mail and delete email forever in Inbox. B-m don't work in
              ;; virtual folder too and move to Gmail/Trash is OK except Inbox label still
              ;; exist, but move to Imap/Trash will remove Inbox label.
              (nnimap "gmail"
                      (nnimap-address "imap.gmail.com")
                      (nnimap-server-port 993)
                      (nnimap-stream ssl))))));;

;; Plato Wu,2009/11/29: It need gnutls get installed.
(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
      ;; Plato Wu,2009/11/30: use ~/.authinfo instead
;;      smtpmail-auth-credentials '(("smtp.gmail.com" 587 "gtalk000@gmail.com" nil))
      smtpmail-auth-credentials my-authinfo
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      smtpmail-local-domain nil)

(setq user-full-name "Plato Wu")
(setq user-mail-address "gtalk000@gmail.com")

;; Plato Wu,2009/11/30: nnimap can not support Chinese Labels
;; Plato Wu,2009/12/05: remove [] for support Gmail system Labels
(setq gnus-ignored-newsgroups "^to\\.\\|^[0-9. 	]+\\( \\|$\\)\\|^[\"]\"[#'()]")

;save sent news & mail
;; Plato Wu,2009/11/11: nnfolder have changed to nnfolder+archive
(setq gnus-message-archive-group
      '((if (message-news-p)
            ;; Plato Wu,2009/12/09: CC to my gmail, so don't need keep it locally
            ;;'("nnfolder+archive:mail.sent.news" "nnimap+gmail:INBOX")
            "nnimap+gmail:INBOX"
          ;; Plato Wu,2009/11/29: Now I use IMAP stytle, so do not need it.
          ;;"nnfolder+archive:mail.sent.mail"
          )))

;; (setq gnus-message-archive-group
;;       '((if (message-news-p)
;;             "nnfolder:mail.sent.news"
;;           "nnfolder:mail.sent.mail")))


;view html email's plain text part.
;(eval-after-load "mm-decode"
;  '(progn
;     (add-to-list 'mm-discouraged-alternatives "text/html")
;     (add-to-list 'mm-discouraged-alternatives "text/richtext")))


;support offline function
(gnus-agentize)

;just work when run firstly
;; (setq gnus-default-subscribed-newsgroups
;;   '("gnu.emacs.help"     ;; 
;;     "comp.lang.scheme" 
;;     "cn.comp.os.linux"
;;     )) ;; 

;(set-language-environment 'Chinese-GB)
;; Plato Wu,2011/10/29: Enable it
;this setting will enable guns can view gbk.
(setq gnus-default-charset 'chinese-iso-8bit
   gnus-group-name-charset-group-alist '((".*" . chinese-iso-8bit))
   gnus-summary-show-article-charset-alist
       '((1 . chinese-iso-8bit)
         (2 . gbk)
         (3 . big5)
         (4 . utf-8))
   gnus-newsgroup-ignored-charsets
       '(unknown-8bit x-unknown iso-8859-1))

;;; set for using GBK in emacs 21/22 with mule-gbk
(unless (>= (string-to-number (substring emacs-version 0 2)) 23)
 (utf-translate-cjk-load-tables))

(defun prefer-gb2312 ()
  (setq mm-coding-system-priorities
	'(iso-8859-1 gb2312
;; utf-8
		     )))

(defun prefer-utf-8 ()
  (setq mm-coding-system-priorities
	'(iso-8859-1 utf-8)))

;; (setq gnus-posting-styles
;;       '((".*"
;; 	 (name "netawater")
;; 	 (address "standin-newsgroup@tianya.cn")
;; 	 (eval (prefer-utf-8)))
;; 	(".*cn\\.bbs.*"
;; 	 (name "netawater")
;; 	 (address "standin-newsgroup@tianya.cn")
;; 	 (eval (prefer-gb2312)))
;; 	(".*cn\\.comp.*"
;; 	 (name "netawater")
;; 	 (address "standin-newsgroup@tianya.cn")
;; 	 (eval (prefer-gb2312)))
;; 	(".*tw\\.comp"
;; 	 (name "netawater")
;; 	 (address "standin-newsgroup@tianya.cn")
;; 	 (eval (setq mm-coding-system-priorities
;; 		     '(iso-8859-1 big5 utf-8))))
;; 	))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (set-language-environment 'Chinese-GB)
;; (setq gnus-default-charset 'chinese-iso-8bit
;; ;Plato,08/4/2, it does not works in Gnus 5.9.0
;; ;      gnus-group-name-charset-group-alist '((".*" . chinese-iso-8bit))
;;       gnus-summary-show-article-charset-alist
;;       '((1 . chinese-iso-8bit)
;;         (2 . gbk)
;;         (3 . big5)
;;         (4 . utf-8))
;;       gnus-newsgroup-ignored-charsets
;;       '(unkown-8bit x-unkown iso-8859-1))

;; (setq gnus-default-charset 'gbk)
;; (add-to-list 'gnus-group-charset-alist
;; 	     '("\\(^\\|:\\)cn\\>\\|\\" gbk))

(setq gnus-summary-show-article-charset-alist
      '((1 . utf-8)
	(2 . big5)
	(3 . gb2312)
 	(4 . utf-7)))

;; Plato,08/4/2, it does not works in Gnus 5.9.0
;; (setq gnus-group-name-charset-group-alist
;;       '(("\.com\.cn:" . gbk)
;; 	("news\\.newsfan\\.net" . gbk)))

;; (setq gnus-group-name-charset-method-alist
;;       '(((nntp "news.newsfan.net") . gbk)))

;; Plato, 08/08/24: solve unknown gb18030 problem.
(setq gnus-newsgroup-ignored-charsets
      '(unknown-8bit x-unknown x-gbk gb18030))

;; (defalias 'mail-header-encode-parameter 'rfc2047-encode-parameter)
;; (add-to-list 'rfc2047-charset-encoding-alist '(gbk . B))
;; (add-to-list 'rfc2047-charset-encoding-alist '(gb18030 . B))

;; netawater,2008/09/18 To delete
;; (setq gnus-posting-styles
;;       '((".*"
;; 	 (name "netawater")
;; 	 (address "netstandin-002@yahoo.com.cn")
;; 	 (eval (setq mm-coding-system-priorities
;; ;;;		     '(iso-8859-1 utf-8))))
;; 		     '(iso-8859-1 gb2312 utf-8))))
;; 	("^cn\\.comp"
;; 	 (name "netawater")
;; 	 (address "netstandin-002@yahoo.com.cn")
;; 	 (eval (setq mm-coding-system-priorities
;; 		     '(iso-8859-1 gb2312 utf-8))))
;; 	("^tw\\.comp"
;; 	 (name "netawater")
;; 	 (address "netstandin-002@yahoo.com.cn")
;; 	 (eval (setq mm-coding-system-priorities
;; 		     '(iso-8859-1 big5 utf-8))))))

;see email's encoding and reader.
(add-hook 'gnus-startup-hook
          '(lambda () 
             (setq gnus-visible-headers 
                   (concat "^User-Agent:\\|^Content-Type:\\|"
                           "Content-Transfer-Encoding:\\|"
                           "^X-mailer:\\|^X-Newsreader:\\|^X-Sender:\\|" 
                           gnus-visible-headers))))
; 'some)


;; If you set this variable to nil, you'll save both time (when starting
;; and quitting) and space (both memory and disk), but it will also mean
;; that Gnus has no record of which groups are new and which are old, so
;; the automatic new newsgroups subscription methods become meaningless.
;; You should always set `gnus-check-new-newsgroups' to `ask-server' or
;; nil if you set this variable to nil.
(setq gnus-save-killed-list nil)

;; *Non-nil means that Gnus will read the `.newsrc' file.
;; Gnus always reads its own startup file, which is called
;; ".newsrc.eld".  The file called ".newsrc" is in a format that can
;; be readily understood by other newsreaders.  If you don't plan on
;; using other newsreaders, set this variable to nil to save some time on
;; entry.
(setq gnus-read-newsrc-file nil)
;; If `gnus-always-read-dribble-file' is non-`nil', Gnus will read the
;; dribble file on startup without querying the user.

(setq gnus-always-read-dribble-file t)

;reserve old email;it seems not work
(setq gnus-fetch-old-headers t)

;let my own articles be auto filled
(add-hook 'message-mode-hook
	  (lambda ()
            ;; Plato Wu,2014/01/25: orgstruct++-mode is obselete
            ;; (orgstruct++-mode
            ;;   (lambda nil 
            ;;     (setq fill-column 72) 
            ;;     (flyspell-mode 1)
            ;;     ))
            (setq fill-column 72) 
            (flyspell-mode 1)
              (turn-on-auto-fill)))

;let other's articles be auto filled
(add-hook 'gnus-article-prepare-hook
          (lambda ()
            (setq fill-column 79)
            (gnus-article-fill-long-lines)))

;remove confirmation for large newsgroup
(setq gnus-large-newsgroup nil)

;remove non printable char warning in Gnus.
(autoload 'ansi-color-apply-on-region "ansi-color")
(defun article-treat-ansi-sequences ()
  "Translate ANSI SGR control sequences into overlays or extents."
  (interactive)
  (save-excursion
    (when (article-goto-body)
      (let ((inhibit-read-only t))
        (ansi-color-apply-on-region (point) (point-max))))))
(add-hook 'gnus-part-display-hook 'article-treat-ansi-sequences)


;; scored my postings.
(add-hook 'message-sent-hook 'gnus-score-followup-article)
(add-hook 'message-sent-hook 'gnus-score-followup-thread)

;;highlight post whose score > 1000
;; (defface benny-gnus-indirect-fup-face nil
;;   "Use this face to display indirect fups to my postings")
;; (set-face-attribute 'benny-gnus-indirect-fup-face nil :foreground "yellow"
;; :weight 'bold)

;; Plato.08/5/16: to do error! need to solve it.
;; (add-to-list 'gnusx-summary-highlight
;;              '((and (>= 1800 score) (>= score 1000))
;; 	       benny-gnus-indirect-fup-face))

;; Let summary window occupy 50% of screen.
(gnus-add-configuration
 '(article
     (cond
      (gnus-use-trees
       '(vertical 1.0
		  (summary 0.5 point)
		  (tree 0.25)
		  (article 1.0)))
      (t
       '(vertical 1.0
		  (summary 0.5 point)
		  (if gnus-carpal '(summary-carpal 4))
		  (article 1.0))))))

;;
