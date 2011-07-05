
;; Set el-get-sources and call el-get to init all those packages we need.

(unless (file-directory-p (expand-file-name "~/.emacs.d/elpa/"))
  (let ((buffer (url-retrieve-synchronously
               "http://tromey.com/elpa/package-install.el")))
  (save-excursion
    (set-buffer buffer)
    (goto-char (point-min))
    (re-search-forward "^$" nil 'move)
    (eval-region (point) (point-max))
    (kill-buffer (current-buffer)))))

;;; This was installed by package-install.el.
;;; This provides support for the package system and
;;; interfacing with ELPA, the package archive.
;;; Move this code earlier if you want to reference
;;; packages in your .emacs.
(when
    (load
     (expand-file-name "~/.emacs.d/elpa/package.el"))
  (package-initialize))

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(if (require 'el-get nil t)
    (progn
      (setq el-get-recipe-path 
            (cons "~/linux-initial/emacs/recipes/" el-get-recipe-path))

      ;; Plato Wu,2011/05/07: remove it for require will search it first for package,
      ;; and the recipe file name is the same as package file name
      (setq load-path 
            (remove (expand-file-name "~/.emacs.d/el-get/el-get/recipes") load-path))
      ;; Plato Wu,2010/12/26: features don't works for elpa type
      (setq el-get-sources
            ;; Plato Wu,2011/01/23: It report Package el-get failed to install, remove it first.
            ;; so remove el-get from el-get-sources
            '(;nxhtml
                                        ;(:name dictionary-el    :type apt-get)
                                        ;(:name emacs-goodies-el :type apt-get)
              vkill 
              ;; Plato Wu,2011/02/24: ido will add ido-configuration into after-load-alist
              ;; which cause error, so must use features.
              (:name ido-hacks :features ido-hacks :after ido-configuration) 
              (:name magit :features magit
                     :after (lambda () (global-set-key (kbd "C-x C-z") 'magit-status)))
              (:name paredit :features paredit :after paredit-configuration)
              (:name htmlize :type elpa :features htmlize)
              ;; Plato Wu,2011/01/03: when I start emacs as a daemon, it require ImageMagick
              ;; get installed to pass error.
              (:name muse :type elpa :features muse :after muse-configuration)
                                        ;        (:name xml-rpc :type elpa)
              ;; Plato Wu,2011/01/30: both lisppaste and weblogger require xml-rpc, el-get can't
              ;; deal with correctly, it report xml-rpc existed when try to install weblogger after
              ;; lisppaste, so disable lisppaste first, it is not useful for me.
                                        ;	(:name lisppaste :type elpa)        
              (:name smart-tab :features smart-tab :after smart-tab-configuration)
              (:name sawfish :features sawfish :after sawfish-configuration)
              (:name google-c-style :features google-c-style
                     :after (lambda () (add-hook 'c-mode-common-hook 'google-set-c-style)))
              (:name psvn :features psvn :after psvn-configuration)
              (:name dired-single :features dired-single :after dired-single-configuration)
              (:name ascii :after ascii-configuration)
              ;; Plato Wu,2011/05/15: vi can be used in it.
              (:name multi-term :features multi-term)
              ;; Plato Wu,2011/05/23: quicklisp will install slime
                                        ;        (:name slime :after slime-configuration)
              (:name cldoc :features cldoc :after cldoc-configuration)))

      (if (not (is-system "cygwin"))
          (progn (setq el-get-sources
                       (append el-get-sources
                               '((:name auctex :after auctex-configuration)
                                 (:name emms :type elpa :features emms :after emms-configuration)
                                 ;; Plato Wu,2011/07/02: it seems there is a problem with session
                                 ;; recipe in el-get
;                                 (:name session :features session :after session-configuration)
                                 redshank dpans2texi)))
                 (when (executable-find "w3m") 
                   (setq el-get-sources
                         (append el-get-sources
                                 '((:name emacs-w3m :features w3m
                                          :after w3m-configuration)
                                   (:name weblogger :type elpa :features weblogger :after blogger-configuration) 
                                   (:name org-toodledo :features org-toodledo :after org-toodledo-configuration))))))
        (setq el-get-sources
              (append el-get-sources
                      '((:name org-toodledo :features org-toodledo :after org-toodledo-configuration)))))
      (setq my-packages
            (append
             '(el-get)
             (mapcar 'el-get-source-name el-get-sources)))
      (el-get 'wait my-packages))
  (url-retrieve
   "http://github.com/dimitri/el-get/raw/master/el-get-install.el"
   (lambda (s)
     (end-of-buffer)
     (eval-print-last-sexp))))

;; Plato Wu, 2010/12/17, this function should not use el-get-dir which is conflict with
;; el-get package itself
;; (let ((el-get-install-dir        (expand-file-name "~/.emacs.d/el-get/"))
;;       (package           "el-get"))
;;   (unless (file-directory-p (concat el-get-install-dir package))
;;     (let* ((bname             "*el-get bootstrap*")
;;            (dummy             (unless (file-directory-p el-get-install-dir)
;;                                 (make-directory el-get-install-dir t)))
;;            (pdir              (concat (file-name-as-directory el-get-install-dir) package))
;;            (git               (or (executable-find "git") (error "Unable to find `git'")))
;; ; Plato Wu,2010/10/11: use sslVerify = false in git config skip https verification in http clone
;;            (url               "https://github.com/dimitri/el-get.git")
;;            (el-get-sources    `((:name ,package :type "git" :url ,url :features el-get :compile "el-get.el")))
;;            (default-directory el-get-install-dir)
;;            (process-connection-type nil) ; pipe, no pty (--no-progress)
;;            (status            (call-process git nil bname t "--no-pager" "clone" "-v" url package)))
;;       (set-window-buffer (selected-window) bname)
;;       (when (eq 0 status)
;;         (load (concat (file-name-as-directory pdir) package ".el"))
;;         ;; (require 'bytecomp)
;;         (el-get-init "el-get")
;;         (with-current-buffer bname
;;           (goto-char (point-max))
;;           (insert "\nCongrats, el-get is installed and ready to serve!"))))))

;; (let ((default-directory "~/.emacs.d/"))
;;   (setq load-path (cons default-directory load-path))
;;   (normal-top-level-add-subdirs-to-load-path))

(provide 'el-get-package)
