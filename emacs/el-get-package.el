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

;; Plato Wu,2010/12/17, this function should not use el-get-dir which is conflict with
;; el-get package itself
(let ((el-get-install-dir        (expand-file-name "~/.emacs.d/el-get/"))
      (package           "el-get"))
  (unless (file-directory-p (concat el-get-install-dir package))
    (let* ((bname             "*el-get bootstrap*")
           (dummy             (unless (file-directory-p el-get-install-dir)
                                (make-directory el-get-install-dir t)))
           (pdir              (concat (file-name-as-directory el-get-install-dir) package))
           (git               (or (executable-find "git") (error "Unable to find `git'")))
           (url               "https://github.com/dimitri/el-get.git")
           (el-get-sources    `((:name ,package :type "git" :url ,url :features el-get :compile "el-get.el")))
           (default-directory el-get-install-dir)
           (process-connection-type nil) ; pipe, no pty (--no-progress)
           (status            (call-process git nil bname t "--no-pager" "clone" "-v" url package)))
      (set-window-buffer (selected-window) bname)
      (when (eq 0 status)
        (load (concat (file-name-as-directory pdir) package ".el"))
        ;; (require 'bytecomp)
        (el-get-init "el-get")
        (with-current-buffer bname
          (goto-char (point-max))
          (insert "\nCongrats, el-get is installed and ready to serve!"))))))

(let ((default-directory "~/.emacs.d/"))
  (setq load-path (cons default-directory load-path))
  (normal-top-level-add-subdirs-to-load-path))

(require 'el-get)

;; Plato Wu,2010/12/26: features don't works for elpa type
(setq el-get-sources
      '(el-get nxhtml yasnippet vkill 
        (:name ido-hacks :after ido-configuration)
        (:name magit
               :after (lambda () (global-set-key (kbd "C-x C-z") 'magit-status)))
        (:name paredit :type elpa :after paredit-configuration)
        (:name emms :type elpa :after emms-configuration)
        (:name htmlize :type elpa)
        ;; Plato Wu,2011/01/03: when I start emacs as a daemon, it require ImageMagick
        ;; get installed to pass error.
        (:name muse :type elpa)
        (:name xml-rpc :type elpa)
	(:name lisppaste :type elpa)
        (:name weblogger :type elpa :after blogger-configuration)
        (:name smart-tab :type elpa :after 'smart-tab-configuration)))
        
(el-get 'sync)

;(blogger-configuration)

(provide 'el-get-package)
