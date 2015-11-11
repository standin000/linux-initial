;; This file only record configuration of Emacs itself. Of course it will
;; call other modules.

(setq load-path (cons "~/linux-initial/emacs/" load-path))

(require 'my-utility)
(require 'my-packages)
(require 'el-get-package)
(require 'my-interface)

;; Plato Wu,2014/09/01: if kill-emacs don't exit all process, use desktop-save first.
;; Plato Wu,2009/06/04: If ido is mess, try to use ido-wash-history and set ido-work-file-list to nil
;; Plato Wu,2013/07/05: if .ido.last is mess or bring sudo buffer at the beginning, 
;; clear it when emacs is closed

