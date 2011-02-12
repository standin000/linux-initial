(defmacro try-function (function)
  `(if (functionp (car ,function))
      (eval ,function)
    'NG))

(defmacro try-require (library)
  `(try-function '(require ,library)))

(defmacro try-require (library)
  (try-function require library))


(defun is-system (type)
  (string= system-type type))

(defun is-version (no)
  (string= (substring emacs-version 0 2) (number-to-string no)))

(defun mycomment (arg)
  (interactive "*P")
  (comment-dwim arg)
  (insert (user-full-name) (format-time-string ",%Y/%m/%d: " (current-time))))


;; Plato Wu,2010/04/14: tramp useage
;; /ssh:username@host:/path
;; it can not work in this version of tramp
;; (add-to-list 'tramp-default-proxies-alist
;; 	     '("remote_hostname" "\\`root\\'" "/ssh:%h:"))

;; tramp need perl or other encode or decode tools.
;; Plato Wu,2009/06/04: sudo: is NG in tramp but sudo:: is OK. because sudo::
;; is shortend syntax for root@localhost
;; Plato,08/08/23: it need add NOPASSWD: /bin/bash, into /etc/sudoers.
;; Plato Wu,2010/01/17: If there is pscp(search by executable-find), tramp
;; will use it, so pscp must be at exec-path
;; Plato Wu,2010/04/06: make sure putty use xterm-256color, so tramp can works
;; in putty
;; Plato Wu,2010/05/23: use list-colors-display to confirm if emacs can use 256 colors
(defun xwl-revert-buffer-with-sudo ()
  "Revert buffer using tramp sudo.
    This will also reserve changes already made by a non-root user."
  (interactive)
  (let ((f (buffer-file-name)))
    (when f
      (let ((content (when (buffer-modified-p)
		       (widen)
		       (buffer-string))))
	  ;; Plato, 08/08/06: remarks for permitting swtich into root mode first,
          ;; some root's file can not be read.
;; 	(if (file-writable-p f)
;; 	    (revert-buffer)
	  (kill-buffer (current-buffer))
	  (if (file-remote-p f)
	      (find-file
	       (replace-regexp-in-string "^\\/[^:]+:" "/sudo:" f))
	    (find-file (concat "/sudo::" f)))
	  (when content
	    (let ((buffer-read-only nil))
	      (erase-buffer)
	      (insert content)))))))

(global-set-key (kbd "C-c s") 'xwl-revert-buffer-with-sudo)

;; Plato Wu,2010/09/13: ignore annoy query.
(defun save-buffers-kill-client (&optional arg)
  "Offer to save each buffer, if there is only client,
   kill Emacs itself, otherwise kill the current connection"
  (interactive "P")
  (if (> (length server-clients) 1)
      (server-save-buffers-kill-terminal arg)
    (progn 
      (remove-hook 'kill-emacs-query-functions 'server-kill-emacs-query-function)
      (if (functionp 'quit-slime)
          (quit-slime))
      (if (and (functionp 'gnus-alive-p)
               (gnus-alive-p))
          (gnus-group-exit))
      (save-buffers-kill-emacs arg))))

;; Plato Wu,2008/10/08 "^X^C" is not "" which is inputed by C-q C-x C-q C-c
(unless (or (is-system "cygwin") window-system)
  (define-key global-map "" 'save-buffers-kill-client))

(defun my-done ()
    (interactive) 
    (server-edit)
    (make-frame-invisible nil t))

(unless (or (is-system "cygwin") 
            (is-version 21))
  (if (is-system "windows-nt")
      (global-set-key (kbd "C-z") 'my-done)
    (global-set-key "\C-z" 'delete-frame)))

;; Plato Wu,2011/01/23: it is need by blogger-configuration and org-toodledo-configuration
(require 'netrc)

(provide 'my-utility)