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
  
(let ((el-get-dir        (expand-file-name "~/.emacs.d/el-get/"))
      (package           "el-get"))
  (unless (file-directory-p (concat el-get-dir package))
    (let* ((bname             "*el-get bootstrap*")
           (dummy             (unless (file-directory-p el-get-dir)
                                (make-directory el-get-dir t)))
           (pdir              (concat (file-name-as-directory el-get-dir) package))
           (git               (or (executable-find "git") (error "Unable to find `git'")))
           (url               "https://github.com/dimitri/el-get.git")
           (el-get-sources    `((:name ,package :type "git" :url ,url :features el-get :compile "el-get.el")))
           (default-directory el-get-dir)
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

(setq el-get-sources
      '(nxhtml yasnippet
        (:name magit
               :after (lambda () (global-set-key (kbd "C-x C-z") 'magit-status)))
        (:name lisppaste :type elpa)
        (:name paredit :type elpa)))
        

;;(el-get 'sync) ; that could/should be (el-get 'sync)
(provide 'el-get-package)
