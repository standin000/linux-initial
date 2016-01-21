;; Plato Wu,2014/04/20: el-get need git, make, mercurial, subversion, cvs
(unless (is-version 24)
  (unless (file-directory-p (expand-file-name "~/.emacs.d/elpa/"))
    ;; Plato Wu,2015/05/26: this site is obsolete for it only support single elpa archive
    ;; (let ((buffer (url-retrieve-synchronously
    ;;                "http://tromey.com/elpa/package-install.el"
    ;;            )))
    ;;   (save-excursion
    ;;     (set-buffer buffer)
    ;;     (goto-char (point-min))
    ;;     (re-search-forward "^$" nil 'move)
    ;;     (eval-region (point) (point-max))
    ;;     (kill-buffer (current-buffer))))
    (mkdir "~/.emacs.d/elpa/")
    (url-copy-file "http://git.savannah.gnu.org/gitweb/?p=emacs.git;a=blob_plain;hb=ba08b24186711eaeb3748f3d1f23e2c2d9ed0d09;f=lisp/emacs-lisp/package.el" "~/.emacs.d/elpa/package.el")
    )
  (load (expand-file-name "~/.emacs.d/elpa/package.el")))

(package-initialize)
;; (setq package-archives '(("melpa" . "http://melpa.org/packages/")
;;  ("gnu" . "http://elpa.gnu.org/packages/")
;;  ("ELPA" . "http://tromey.com/elpa/")
;;  ("marmalade" . "http://marmalade-repo.org/packages/")
;;  ("SC" . "http://joseito.republika.pl/sunrise-commander/")))

;; Plato Wu,2015/02/28: for communitiy elpa 
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       ;; Plato Wu,2014/03/18: nintfloor don't support TLS
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (end-of-buffer)
    (eval-print-last-sexp)))

(setq el-get-use-autoloads nil)

(add-to-list 'el-get-recipe-path "~/linux-initial/emacs/recipes/")

;; Plato Wu,2011/05/07: remove it for require will search it first for package,
;; and the recipe file name is the same as package file name; 2015/03/02: it is obsolete now
;; (setq load-path 
;;       (remove (expand-file-name "~/.emacs.d/el-get/el-get/recipes") load-path))


;; Plato Wu,2013/06/07: @todo
;; Plato Wu,2011/05/19: we must change the usage of el-get, don't use :after option to
;; call package configuration, but run it after el-get finished its task. since el-get
;; use eval-after-load to call :after function, it is annoy style.

;; Set el-get-sources and call el-get to init all those packages we need.
(if ;(is-version 24)
    (> (compare-version "24.4") 0)
    ;; Plato Wu,2015/01/27: clojure-mode in raspberry need emacs 24
    (setq el-get-sources 
          ;; Plato Wu,2015/04/04: clojure-mode is NG in cygwin & ninthfloor.org
          ;'((clojure-mode :type elpa))
          '((:name helm :type elpa)
            (:name helm-projectile :type elpa)))
  ;; Plato Wu,2013/06/13: emacs below 24.3 need it
  (setq el-get-sources
        '((:name cl-lib :type elpa))))

(setq el-get-sources 
      (append el-get-sources 
              '((:name magit :type elpa
                       (progn
                         (global-set-key (kbd "C-x C-z") 'magit-status)
                         ;; Plato Wu,2015/06/01: stop 1.4.0 auto revert.
                         (setq magit-auto-revert-mode nil)
                         ;; Plato Wu,2015/06/01: stop 1.4.0 auto revert warning.
                         (setq magit-last-seen-setup-instructions "1.4.0")))
                (:name projectile :type elpa)
                ;;Plato Wu,2015/06/05: for org package, when installing from ELPA, please do so from a fresh Emacs
                ;;session where no org function has been called, use list-packages and press d & x to delete old version
                ;; and i & x to install new one.
;                (:name org :type elpa)
                (:name paredit :type elpa)
                ;;        (:name smart-tab (smart-tab-configuration))
                (:name google-c-style :type elpa)
                (:name psvn :type elpa)
                (:name ascii :type elpa)
                ;; Plato Wu,2015/02/28: it is obsolete?
                (:name smart-compile (compile-configuration))
                ;;        (:name ggtags :type elpa :features ggtags (ggtags-configuration))

                (:name popup :type elpa)
                (:name auto-complete :type elpa)

                ;; (:name nxhtml)
                ;; (:name dictionary-el    :type apt-get)
                ;; (:name emacs-goodies-el :type apt-get)
                ;; Plato Wu,2011/01/30: both lisppaste and weblogger require xml-rpc, el-get can't
                ;; deal with correctly, it report xml-rpc existed when try to install weblogger after
                ;; lisppaste, so disable lisppaste first, it is not useful for me.
                ;; (:name lisppaste :type elpa)        
                )))

(when (not (is-system "cygwin"))
  (setq el-get-sources
        (append el-get-sources
                '((:name xclip :type elpa 
                         (progn
                             (when (getenv "DISPLAY")
                               (autoload 'turn-on-xclip "xclip" "exchange clip between X and emacs" t nil)
                               (turn-on-xclip)))) 
                  ;; Plato Wu,2014/03/18: no this package in the latest el-get
;                  (:name nrepl) 
      ;           (:name auctex auctex-configuration)
                  (:name htmlize :type elpa)
                  (:name muse :type elpa)
                  ;; Plato Wu,2015/05/11: no this package in emacs of raspberrypi
;                  (:name sawfish :type elpa (sawfish-configuration))
                  ;; Plato Wu,2014/03/18: it is very old, need a good replacement
                 ;(:name dired-single :features dired-single (dired-single-configuration))
                  (:name multi-term :type elpa)
                  ;; Plato Wu,2011/07/02: it seems there is a problem with session
                  ;; recipe in el-get
      ;            (:name session :features session session-configuration)
                  ;;A collection of code-wrangling Emacs macros mostly geared towards Common Lisp, but some are useful for other Lisp dialects, too.
                  (:name redshank :type elpa) 
                  (:name vkill :type elpa)
                  ;; Plato Wu,2015/01/27: no cldoc now in el-get?
;                  (:name cldoc)
      ;           (:name dpans2texi)
                  )))
  (when (executable-find "w3m") 
    (setq el-get-sources
          (append el-get-sources
                  '((:name w3m (w3m-configuration))
                    ;; Plato Wu,2011/01/03: when I start emacs as a daemon, it require ImageMagick
                    ;; get installed to pass error.
                    (:name xml-rpc :type elpa)
                    (:name weblogger :type elpa 
                           (eval-after-load "muse-mode" '(blogger-configuration)))))))
  (when (executable-find "mpd")
    (setq el-get-sources
          (append el-get-sources
                  '((:name emms :type elpa (emms-configuration)))))))

(mapcar #'(lambda (package)
             (unless 
                 (or 
                  (el-get-package-is-installed (cadr package))
                  (member (symbol-name (cadr package)) (el-get-read-all-recipe-names))
                  )
                 ;; Plato Wu,2013/06/03: emacs 24 support ELPA natively, but it need refresh package list
                 (package-refresh-contents))
               (eval (cons 'el-get-bundle! (cdr package))) 
               )
         el-get-sources)
;; advice-add need 24.4
(if (> (compare-version "24.4") 0)
    (progn 
        ;; Plato Wu,2015/05/26: helm need emacs 24.4
      (my-helm-configuration)
      (projectile-configuration))
    (ido-configuration)
  )

(paredit-configuration)
(org-configuration)
(c-mode-configuration)
(auto-complete-configure)

;; el-get allows you to install and manage elisp code for Emacs. It supports lots of differents types of sources (git, svn, apt, elpa, etc) and is able to install them, update them and remove them, but more importantly it will init them for you.

;; That means it will require the features you need, load the necessary files, set the Info paths so that C-h i shows the new documentation you now depend on, and finally call your own :post-init function for you to setup the extension. Or call it a package.

;; you can now easily checkout a stable branch from a
;; git repository (thanks to the :checkout property) and you can even
;; setup which checksum you want installed.

;(el-get-bundle 'magit :type elpa (global-set-key (kbd "C-x C-z") 'magit-status))
;(macroexpand '(el-get-bundle 'magit :type elpa))
;(el-get-elpa-symlink-package 'epl)
;; (defun package-dependency (name)
;;   (package-compute-transaction (list name)
;;                                (package-desc-reqs (cdr (assq name package-archive-contents)))))
;; (package-desc-reqs (cdr (assq 'projectile package-archive-contents)))


;; ;; ;; Plato Wu,2015/02/28: Transfer all packages to new emacs
;; ;; ;; (setq my-packages
;; ;; ;;               ',(mapcar #'el-get-as-symbol
;; ;; ;;                         (el-get-list-package-names-with-status "installed")))

(provide 'el-get-package)

;; (defvar prelude-packages
;;   '(ack-and-a-half auctex clojure-mode coffee-mode deft expand-region
;;                    gist groovy-mode haml-mode haskell-mode inf-ruby
;;                    magit magithub markdown-mode paredit projectile python
;;                    sass-mode rainbow-mode scss-mode solarized-theme
;;                    volatile-highlights yaml-mode yari zenburn-theme)
;;   "A list of packages to ensure are installed at launch.")

;; (defun prelude-packages-installed-p ()
;;   (loop for p in prelude-packages
;;         when (not (package-installed-p p)) do (return nil)
;;         finally (return t)))

;; (unless (prelude-packages-installed-p)
;;   ;; check for new packages (package versions)
;;   (message "%s" "Emacs Prelude is now refreshing its package database...")
;;   (package-refresh-contents)
;;   (message "%s" " done.")
;;   ;; install the missing packages
;;   (dolist (p prelude-packages)
;;     (when (not (package-installed-p p))
;;       (package-install p))))

;; (provide 'prelude-packages)

;(el-get-clear-status-cache)
