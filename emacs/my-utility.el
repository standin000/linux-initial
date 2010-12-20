(defmacro try-function (function &rest parameters)
  `(if (functionp (quote ,function))
     (apply (quote ,function) (quote ,parameters))
     'NG))

(defun ido-configuration ()
  (require 'ido)
  (setq ido-enable-tramp-completion nil) 
  (setq ido-ignore-buffers
	'("^ .*"
	;; ignore *eshell*, *svn-status*, a awkward regular expression
	;; for I do not know how to perfect match a word which is not "eshell"
	;; or "svn-status".
	  "^\\*[^es].\\{3\\}[^s].*"
	  "TAGS"))
  (require 'ido-hacks)
  (ido-mode t))

(defun emms-configuration ()
  (require 'emms-volume)
  (require 'emms-setup)
  (require 'emms-player-mpd)
  (require 'emms-history)
  (require 'emms-score)
  (require 'emms-i18n)
  (emms-standard)
  (emms-default-players)
  ;;run (emms-history-save) first
  (emms-history-load)
  (add-to-list 'emms-player-list 'emms-player-mpd)
  (add-to-list 'emms-info-functions 'emms-info-mpd)
  (emms-player-set emms-player-mpd 'regex
                   "\\.ogg\\|\\.mp3\\|\\.wma\\|\\.ogm\\|\\.asf\\|\\.mkv\\|http://\\|mms://\\|\\.rmvb\\|\\.flac\\|\\.vob\\|\\.m4a\\|\\.ape\\|\\.mpc")
  (emms-player-set emms-player-mplayer 'regex
                   "\\.wav\\|\\.pls+\\|\\.mpg\\|\\.mpeg\\|\\.wmv\\|\\.wma\\|\\.mov\\|\\.avi\\|\\.divx\\|\\.ogm\\|\\.asf\\|\\.mkv\\|.rm\\|\\.rmvb\\|\\.mp4|\\.rm")

  ;; Show the current track each time EMMS
  ;; starts to play a track with "NP : "
  (add-hook 'emms-player-started-hook 'emms-show)
  (setq emms-volume-change-function 'emms-volume-mpd-change)
  (setq emms-show-format "NP: %s")
  (setq emms-player-next-function 'emms-random)
  (setq emms-playlist-buffer-name "*Music*")
  (setq emms-player-mpd-music-directory "~/Music")
  (setq emms-player-next-function 'emms-random)
  (emms-score 1)
  (defun my-stop-player ()
    "Stop emms player."
    (interactive)
    (shell-command "mpd --kill &")
    (emms-playlist-current-kill)
    (emms-player-mpd-disconnect))
  (defun my-start-player ()
    "Start MPD and sync to its playlistemms player."
    (interactive)
    (shell-command "mpd &") ; uses default ~/.mpdconf
    (shell-command "sleep 3 ") 
    (emms-player-mpd-connect)
    (switch-to-buffer emms-playlist-buffer))) 

(provide 'my-utility)