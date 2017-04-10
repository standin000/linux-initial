;;; el-get-package.el ---                         -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Plato Wu

;; Author: Plato Wu <gtalk000@gmail.com>
;; Keywords: 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:n

;; 

;;; Code:

;; Plato Wu,2014/04/20: el-get need git, make, mercurial, subversion, cvs, texinfo(for makeinfo)

;; @todo
    ;;  Control whether el-get should generate autoloads for this
    ;; package. Setting this to nil prevents el-get from generating
    ;; autoloads for the package. Default is t. Setting this to a
    ;; string or a list of string will load the named autoload
    ;; files.

;; `(setq my-packages
;;       ',(mapcar #'el-get-as-symbol
;;                 (el-get-list-package-names-with-status "installed")))
;; Plato Wu,2011/05/19: we must change the usage of el-get, don't use :after option to
;; call package configuration, but run it after el-get finished its task. since el-get
;; use eval-after-load to call :after function, it is annoy style.


; So anyway, if you make a change to el-get-sources, el-get insist use (el-get-package-status-recipes)
; you will have to use either el-get-update or el-get-reinstall on that package
; you can check (el-get-package-def 'magit)
;;
;; (el-get-post-install 'magit) can run after install faile


;(setq my-packages (quote (ascii auto-complete cedet helm helm-projectile magit org-mode package paredit popup projectile psvn smart-compile)))
;; smart-tab, ggtags, nxhtml 
;; org-wunderlist ; csharp-mode of code.google.com is NG  (lambda () (add-to-list 'auto-mode-alist '("\\.cs$" . csharp-mode)))
;; Plato Wu,2016/03/28: magit need popup, auto-complete+ need auto-complete
;; (:name dictionary-el    :type apt-get)
;; (:name emacs-goodies-el :type apt-get)
;; Plato Wu,2011/01/30: both lisppaste and weblogger require xml-rpc, el-get can't
;; deal with correctly, it report xml-rpc existed when try to install weblogger after
;; lisppaste, so disable lisppaste first, it is not useful for me.
;; (:name lisppaste :type elpa)        
;; Plato Wu,2011/05/07: remove it for require will search it first for package,
;; and the recipe file name is the same as package file name; 2015/03/02: it is obsolete now
;; (setq load-path 
;;       (remove (expand-file-name "~/.emacs.d/el-get/el-get/recipes") load-path))

(setq my-packages '(popup paredit psvn ascii smart-compile google-c-style
                          ;(:name org-mode :after (org-mode-configuration))
                          ))

(org-mode-configuration)

(if (higher-version 24.4)
    (progn
      (setq my-packages  
            ;; Plato Wu,2015/04/04: clojure-mode is NG in cygwin & ninthfloor.org
            ;; make sure (el-get-package-or-source 'helm) don't contain helm, then el-get-install 'helm
            ;; Plato Wu,2016/04/06: there is helm-configuration in helm package.
            (append my-packages '(s
				  dash
				  epl
				  pkg-info
				  projectile
				  helm-core
				  (:name helm :after (helm-config) :post-init (require 'helm-config) :type elpa)
  ;				  (:name cedet :after (cedet-configuration)  :features cedet-devel-load  :type elpa)
                  ;; Plato Wu,2016/11/04: need specify all dependes of magit in elpa, or require maigt is not OK in el-get
				  helm-projectile async with-editor git-commit magit-popup magit)))
      ;; Plato Wu,2015/12/07: it will load built-in cedet first, so use cedet-develp-load at features
;      (el-get-bundle 'cedet :features cedet-devel-load (cedet-configuration) :type elpa)
      ;;(featurep 'cedet-devel-load)
      )
  ;; Plato Wu,2013/06/13: emacs below 24.3 need it
  (ido-configuration)
  ;; Plato Wu,2016/09/11: it need git-commit-mode and git-rebase-mode, but they are removed now, copy from old repostory
  (load "~/linux-initial/emacs/recipes/git-commit-mode.el")
  (load "~/linux-initial/emacs/recipes/git-rebase-mode.el")
  (setq my-packages
        (append my-packages '(cl-lib
 			      (:name magit
 				     ;; Plato Wu,2016/09/11: latest magit need emacs 24
 				     :minimum-emacs-version "23"
 				     :after (magit-configuration)
 				     :features magit
 				     :checkout "1.4.2"
 				     :depends (cl-lib)
 				     :compile "magit.*\\.el\\'"
 				     :load-path "."
                     ;; Plato Wu,2017/01/05: build magit docs ng, so clear this step by check these command
                     ;; (el-get-build-commands 'magit) (el-get-install-or-init-info 'magit 'build)
  				     :info nil
  				     :build (progn nil)
                     ;; `(("make" ,(format "EMACSBIN=%s" el-get-emacs) "docs")) 
                     ;; :build/berkeley-unix (("gmake" ,(format "EMACSBIN=%s" el-get-emacs) "docs"))
                     ;; ;; assume windows lacks make and makeinfo
                     ;; :build/windows-nt (progn nil)
                                 )
			      ))))
;; Plato Wu,2016/11/20: auto-complete need cl-lib when < 24.3, so put it after cl-lib.
(setq my-packages
        (append my-packages '(auto-complete)))
  
(when (not (is-system "cygwin"))
  (setq my-packages
        (append my-packages
                '(xclip
                  ;; Plato Wu,2014/03/18: no this package in the latest el-get
                  ; (:name nrepl) 
		  ;; Plato Wu,2016/09/09: auctex NG now
;                  auctex
                  htmlize
                  muse 
                  ;; Plato Wu,2015/05/11: no this package in emacs of raspberrypi
         ;     (:name sawfish :type elpa (sawfish-configuration))
                ;; Plato Wu,2014/03/18: it is very old, need a good replacement
               ;(:name dired-single :features dired-single (dired-single-configuration))
                 multi-term
                 ;; Plato Wu,2011/07/02: it seems there is a problem with session
                  ;; recipe in el-get
                   ;  (:name session :features session session-configuration)
		 ;;A collection of code-wrangling Emacs macros mostly geared towards Common Lisp, but some are useful for other Lisp dialects, too.
		 ;; Plato Wu,2016/09/11: NG now
;;                   redshank
                   vkill
                  ;; Plato Wu,2015/01/27: no cldoc now in el-get?
                ;                  (:name cldoc)
            ;           (:name dpans2texi)
                        )))
(when (executable-find "w3m") 
    (setq my-packages
          (append my-packages
                  '(;Plato Wu,2016/09/09: there is not w3m in el-get
                    (:name w3m :type elpa) 
                    ;; Plato Wu,2011/01/03: when I start emacs as a daemon, it require ImageMagick
                    ;; get installed to pass error.
                    xml-rpc
                    ;; Plato Wu,2016/09/11: NG now
                    ;; Please get a current version from https://launchpad.net/weblogger-el

                    ;; This software is not able to deal with blogs on Blogger.com,
                    ;; as you can read on this thread:
                    ;; http://thread.gmane.org/gmane.emacs.blogging/76
                    ;;weblogger
		    ))))
  (when (executable-find "mpd")
    (setq my-packages
          (append my-packages
                  '(emms)))))
;; (mapcar #'(lambda (package)
;;              (unless 
;;                  (or 
;;                   (el-get-package-is-installed (cadr package))
;;                   (member (symbol-name (cadr package)) (el-get-read-all-recipe-names))
;;                   )
;;                  ;; Plato Wu,2013/06/03: emacs 24 support ELPA natively, but it need refresh package list
;;                  (package-refresh-contents))
;;                (eval (cons 'el-get-bundle! (cdr package))) 
;;                ))

(progn
 (setq el-get-sources
       (mapcar
        (lambda (pkg-name)
          (if (listp pkg-name)
              pkg-name
            `(:name ,pkg-name
                    :after ,(let ((after-func (intern (concat (symbol-name pkg-name) "-configuration"))))
                              (if (functionp after-func)
                                  (list after-func)
                                nil))
                    :post-init
		    (condition-case nil
			(require (quote ,pkg-name))
		      (error (message "%s is not existed" (quote ,pkg-name)) nil))
		    ;; Plato Wu,2016/10/26: elpa method don't support features
                    :features (,pkg-name)
                    :type elpa)))
        my-packages))
; ensures that any currently installed packages will be initialized and any
; required packages will be installed.
 (el-get 'sync
         (mapcar
          (lambda (pkg-name)
            (if (listp pkg-name)
                (plist-get pkg-name :name)
              pkg-name
              ))
          my-packages))
 )
 ;; (require 'el-get-bundle)

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
;;; el-get-packagenew.el ends here
