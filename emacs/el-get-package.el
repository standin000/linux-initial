;; Plato Wu,2014/04/20: el-get need git, make, mercurial, subversion, cvs
;; Plato Wu,2013/06/03: emacs 24 support ELPA natively
;; (if (is-version 24)
;;   (package-initialize)
;;   (unless (file-directory-p (expand-file-name "~/.emacs.d/elpa/"))
;;     (let ((buffer (url-retrieve-synchronously
;;                "http://tromey.com/elpa/package-install.el")))
;;       (save-excursion
;;         (set-buffer buffer)
;;         (goto-char (point-min))
;;         (re-search-forward "^$" nil 'move)
;;         (eval-region (point) (point-max))
;;         (kill-buffer (current-buffer)))))
;;   (load (expand-file-name "~/.emacs.d/elpa/package.el"))
;;   (package-initialize))

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       ;; Plato Wu,2014/03/18: nintfloor don't support TLS
       "http://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(add-to-list 'el-get-recipe-path "~/linux-initial/emacs/recipes/")

;; Plato Wu,2011/05/07: remove it for require will search it first for package,
;; and the recipe file name is the same as package file name
(setq load-path 
      (remove (expand-file-name "~/.emacs.d/el-get/el-get/recipes") load-path))
;; Plato Wu,2013/06/07: @todo
;; Plato Wu,2011/05/19: we must change the usage of el-get, don't use :after option to
;; call package configuration, but run it after el-get finished its task. since el-get
;; use eval-after-load to call :after function, it is annoy style.

;; Set el-get-sources and call el-get to init all those packages we need.
(unless (is-version 24)
  ;; Plato Wu,2013/06/13: emacs below 24.3 need it
  (setq el-get-sources
        '((:name cl-lib :type elpa))))


(setq el-get-sources
      (append 
       el-get-sources
      '((:name magit :after (global-set-key (kbd "C-x C-z") 'magit-status))
        (:name org-mode :after 
               (eval-after-load "org" '(org-configuration)))
        (:name paredit :after (paredit-configuration))
;;        (:name smart-tab :after (smart-tab-configuration))
        (:name google-c-style :after (c-mode-configuration))
        (:name psvn :after (psvn-configuration))
        (:name ascii :after (ascii-configuration))
        (:name smart-compile :after (compile-configuration))
;;        (:name ggtags :type elpa :features ggtags :after (ggtags-configuration))
        (:name auto-complete :features auto-complete-config :after (auto-complete-configure))
      ;; (:name nxhtml)
      ;; (:name dictionary-el    :type apt-get)
      ;; (:name emacs-goodies-el :type apt-get)
      ;; Plato Wu,2011/01/03: when I start emacs as a daemon, it require ImageMagick
      ;; get installed to pass error.
      ;; (:name xml-rpc :type elpa)
      ;; Plato Wu,2011/01/30: both lisppaste and weblogger require xml-rpc, el-get can't
      ;; deal with correctly, it report xml-rpc existed when try to install weblogger after
      ;; lisppaste, so disable lisppaste first, it is not useful for me.
      ;; (:name lisppaste :type elpa)        
        )))

(when (not (is-system "cygwin"))
  (setq el-get-sources
        (append el-get-sources
                '(;; Plato Wu,2013/05/27: TODO
                  (:name xclip :after 
                         (progn
                             (when (getenv "DISPLAY")
                               (autoload 'turn-on-xclip "xclip" "exchange clip between X and emacs" t nil)
                               (turn-on-xclip)))) 
                  (:name clojure-mode :type elpa)
      ;           (:name clojure-test-mode :type elpa)
                  ;; Plato Wu,2014/03/18: no this package in the latest el-get
;                  (:name nrepl) 
      ;           (:name auctex :after auctex-configuration)
                  (:name htmlize :type elpa)
                  (:name muse :type elpa)
                  (:name sawfish :after (sawfish-configuration))
                  ;; Plato Wu,2014/03/18: it is very old, need a good replacement
                 ;(:name dired-single :features dired-single :after (dired-single-configuration))
                  (:name multi-term)
                  ;; Plato Wu,2011/07/02: it seems there is a problem with session
                  ;; recipe in el-get
      ;            (:name session :features session :after session-configuration)
                  (:name redshank) 
                  (:name vkill)
                  (:name cldoc)
      ;           (:name dpans2texi)
                  )))
  (when (executable-find "w3m") 
    (setq el-get-sources
          (append el-get-sources
                  '((:name emacs-w3m :after (w3m-configuration))
                    (:name weblogger :type elpa :after 
                           (eval-after-load "muse-mode" '(blogger-configuration)))))))
  (when (executable-find "mpd")
    (setq el-get-sources
          (append el-get-sources
                  '((:name emms :type elpa :after (emms-configuration)))))))
(setq my-packages
      (append
       '("el-get")
       (mapcar 'el-get-source-name el-get-sources)))

(el-get 'sync my-packages)

(provide 'el-get-package)

