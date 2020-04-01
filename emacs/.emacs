;; This file only record configuration of Emacs itself. Of course it will
;; call other modules.

;; Plato Wu,2019/01/15: for .dir-locals.el
;; ((nil .
;;       ((eval . (progn
;;                  (setq tags-file-name (concat (locate-dominating-file default-directory "TAGS") "TAGS"))
;;                  ;; (setq tags-file-name "TAGS")
;;                  ;; (let ((i 0))
;;                  ;;   (while (and (not (file-exists-p tags-file-name))
;;                  ;;               (< i 5))
;;                  ;;     (setq tags-file-name (concat "../" tags-file-name))
;;                  ;;     (setq i (+ 1 i)))
;;                  ;;   (if (= i 5) (setq tags-file-name nil)))
;;                  )))))

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(setq load-path (cons "~/linux-initial/emacs/" load-path))

(require 'my-utility)
(require 'my-packages)
;;(require 'el-get-package)
(require 'my-interface)

;; Plato Wu,2014/09/01: if kill-emacs don't exit all process, use desktop-save first.
;; Plato Wu,2009/06/04: If ido is mess, try to use ido-wash-history and set ido-work-file-list to nil
;; Plato Wu,2013/07/05: if .ido.last is mess or bring sudo buffer at the beginning, 
;; clear it when emacs is closed
