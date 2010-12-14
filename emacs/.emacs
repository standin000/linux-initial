;; This file only record configuration of Emacs itself. Of course it will
;; call other modules.

(setq load-path (cons "~/linux-initial/emacs/" load-path))

(require 'my-utility)
(require 'my-interface)
(require 'el-get-package)