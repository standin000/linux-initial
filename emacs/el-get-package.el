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
(unless (> (compare-version "24") 0)
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

;(package-initialize)
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
;; no default recipes
;; (unless (require 'el-get nil 'noerror)
;;   (require 'package)
;;   (add-to-list 'package-archives
;;                '("melpa" . "http://melpa.org/packages/"))
;;   (package-refresh-contents)
;;   (package-initialize)
;;   (package-install 'el-get)
;;   (require 'el-get))

(unless (require 'el-get nil 'noerror)
  (url-retrieve
   ;; Plato Wu,2014/03/18: ninthfloor don't support TLS
   "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el"
   (lambda (s)
     (goto-char (point-max))
        (eval-print-last-sexp))))
; install process running at background, so this report error when first time.
(add-to-list 'el-get-recipe-path "~/linux-initial/emacs/recipes/")

;; `(setq my-packages
;;       ',(mapcar #'el-get-as-symbol
;;                 (el-get-list-package-names-with-status "installed")))
                                        ;(setq my-packages (quote (ascii auto-complete cedet helm helm-projectile magit org-mode package paredit popup projectile psvn smart-compile)))

;org-wunderlist ; csharp-mode of code.google.com is NG  
;; Plato Wu,2016/03/28: magit need popup, auto-complete+ need auto-complete
(setq my-packages '(popup magit org-mode paredit google-c-style psvn ascii smart-compile auto-complete))

(if (> (compare-version "24.4") 0)
    (progn
      (setq my-packages  
           ;; Plato Wu,2015/04/04: clojure-mode is NG in cygwin & ninthfloor.org
            (append my-packages '(projectile ))))
  ;; Plato Wu,2013/06/13: emacs below 24.3 need it
  (ido-configuration)
  (setq my-packages
        (append my-packages '(cl-lib))))

(setq el-get-sources
      (mapcar
       (lambda (pkg-name)
         `(:name ,pkg-name
           :after ,(let ((after-func (intern (concat (symbol-name pkg-name) "-configuration"))))
				     (if (functionp after-func)
					 (list after-func)
                     nil)))) 
       my-packages))

(when (not (is-system "cygwin"))
  (setq my-packages
        (append my-packages
                '(xclip
                  ;; Plato Wu,2014/03/18: no this package in the latest el-get
                  ; (:name nrepl) 
                  auctex
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
                   redshank
                   vkill
                  ;; Plato Wu,2015/01/27: no cldoc now in el-get?
                ;                  (:name cldoc)
            ;           (:name dpans2texi)
                        )))
  (when (executable-find "w3m") 
    (setq my-packages
          (append my-package
                  '(w3m 
                    ;; Plato Wu,2011/01/03: when I start emacs as a daemon, it require ImageMagick
                    ;; get installed to pass error.
                    xml-rpc
                    weblogger))))
  (when (executable-find "mpd")
    (setq my-packages
          (append my-packages
                  '(emms)))))

;; (setq el-get-sources
;;       (append el-get-sources '(:name cedet :feature )))

; ensures that any currently installed packages will be initialized and any
; required packages will be installed.
(el-get 'sync my-packages)
(require 'el-get-bundle)

; So anyway, if you make a change to el-get-sources, el-get insist use (el-get-package-status-recipes)
; you will have to use either el-get-update or el-get-reinstall on that package
(when (> (compare-version "24.4") 0)
  ; make sure (el-get-package-or-source 'helm) don't contain helm, then el-get-install 'helm
  ;; Plato Wu,2016/04/06: there is helm-configuration in helm package.
  (el-get-bundle helm (helm-config))
  ;; Plato Wu,2016/04/07: helm-projectile need require obviously
  (el-get-bundle! helm-projectile (helm-projectile-configuration))
)
;; Plato Wu,2015/12/07: it will load built-in cedet first, so use cedet-devel-load at features
(el-get-bundle 'cedet :features cedet-devel-load (cedet-configuration))
;;(featurep 'cedet-devel-load)
(provide 'el-get-package)
;;; el-get-packagenew.el ends here
