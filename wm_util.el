;; Util file for EXWM that will be loaded by wm.el
;; Screenshots
(when (executable-find "maim")
  (defun screenshot () (interactive)
	 (let ((tmp-path (format-time-string "/tmp/%F_%X.png"))
	       (default-path (concat (getenv "HOME") (format-time-string
						      "/Pictures/%F_%X.png"))))
	   (shell-command (concat "maim -s '" tmp-path "'"))
	   (if (file-exists-p tmp-path)
	       (let ((chosen-path (read-file-name "Save as: " default-path)))
		 (rename-file tmp-path chosen-path)
		 (if (file-exists-p chosen-path)
		     (message (concat "Saved as" chosen-path))
		   (message "No screenshot was saved")
		   )
		 )
	     (message "No screenshot was saved")
	     )
	   )
	 )
  (bind-key "<print>" 'screenshot)
  )

(when (executable-find "xrandr")
  (setq brightness 1.0)
  (defun brightness_add (to_add)
    "Add brightness, take account of the current brightness (no more than 1.0 or
  less than 0.0)"
    (setq final_brightness (+ brightness to_add))
    (if (and (<= final_brightness 1.0) (>= final_brightness 0.0))
	(progn
	  (shell-command (format "xrandr --output eDP-1 --brightness %f"
				 final_brightness))
	  (setq brightness final_brightness)
	  ))
    (message "Brightness: %d%%" (round (* brightness 100)))
    )
  (defun brightness_inc () (interactive)
	 (brightness_add 0.05))
  (defun brightness_dec () (interactive)
	 (brightness_add -0.05))
  (bind-key "<XF86MonBrightnessDown>" 'brightness_dec)
  (bind-key "<XF86MonBrightnessUp>" 'brightness_inc)
  )

(defcustom lock-screen-cmd ""
  "Command used to lock the screen")

(when (and (executable-find "i3lock") (executable-find "maim"))
  (setq lock-screen-cmd "(maim /tmp/screenlock.png && i3lock -i /tmp/screenlock.png)")
  (defun lock-screen ()
    (interactive)
    (shell-command lock-screen-cmd)
    )
  (bind-key "s-l" 'lock-screen)
  )

;; Volume Control
(when (executable-find "wpctl")
  (setq sound_volume 0)
  (shell-command "wpctl set-volume @DEFAULT_SINK@ 0%")
  (setq sound_mute nil)
  (shell-command "wpctl set-mute @DEFAULT_SINK@ 0")
  (defun sound_volume_add (to_add)
    (setq final_sound_volume (+ sound_volume to_add))
    (when (and (<= final_sound_volume 100) (>= final_sound_volume 0))
          (shell-command (format "wpctl set-volume @DEFAULT_SINK@ %d%%"
				 final_sound_volume))
          (setq sound_volume final_sound_volume)
          )
    (message "Volume: %d%%" sound_volume)
    )
  (defun sound_mute_toggle ()
    (interactive)
    (if sound_mute
	(progn
	  (setq sound_mute nil)
	  (shell-command "wpctl set-mute @DEFAULT_SINK@ 0")
	  (message "Volume: on")
	  )
      (setq sound_mute t)
      (shell-command "wpctl set-mute @DEFAULT_SINK@ 1")
      (message "Volume: mute")
      )
    )
  (defun sound_volume_inc () (interactive)
	 (sound_volume_add 5))
  (defun sound_volume_dec () (interactive)
	 (sound_volume_add -5))
  (bind-key "<XF86AudioLowerVolume>" 'sound_volume_dec)
  (bind-key "<XF86AudioRaiseVolume>" 'sound_volume_inc)
  (bind-key "<XF86AudioMute>" 'sound_mute_toggle)
  )

(when (and (executable-find "feh") (file-exists-p "~/.wallpaper.jpg"))
    (shell-command "feh --bg-fill ~/.wallpaper.jpg")
  )

(nconc exwm-input-global-keys
       '(
	 ([XF86MonBrightnessDown] . brightness_dec)
	 ([XF86MonBrightnessUp] . brightness_inc)
	 ([XF86AudioLowerVolume] . sound_volume_dec)
	 ([XF86AudioRaiseVolume] . sound_volume_inc)
	 ([XF86AudioMute] . sound_mute_toggle)
	 ([print] . screenshot)
	 ([?\s-l] . lock-screen)
	 ))

(when (executable-find "systemctl")
  (defun system-poweroff ()
    (interactive)
    "Poweroff using systemctl"
    (when (yes-or-no-p "Poweroff computer?")
      (save-some-buffers)
      (start-process-shell-command "poweroff" nil "systemctl poweroff")))
  (defun system-reboot ()
    (interactive)
    "Reboot using systemctl"
    (when (yes-or-no-p "Reboot computer?")
      (save-some-buffers)
      (start-process-shell-command "reboot" nil "systemctl reboot")))
  (defun system-suspend ()
    (interactive)
    "Suspend using systemctl and lock screen if lock-screen function is defined"
    (shell-command (concat "(" lock-screen-cmd " & systemctl suspend)")))
  (defun system-hibernate ()
    (interactive)
    "Hibernate using systemctl and lock screen if lock-screen function is
  defined"
    (when (yes-or-no-p "Hibernate?")
      (shell-command (concat "(" lock-screen-cmd " & systemctl hibernate)"))))
  
  (defcustom system-exit-char-choice-list
    '(("l" "Logout" save-buffers-kill-terminal)
      ("p" "Poweroff" system-poweroff)
      ("r" "Reboot" system-reboot)
      ("s" "Suspend" system-suspend)
      ("h" "Hibernate" system-hibernate))
    "List of choice used by system-exit-menu, which uses char-choice-menu"
    )

  (defun system-exit-menu ()
    "Use choices from system-exit-char-choice-list to create an exit menu"
    (interactive)
    (char-choice-menu system-exit-char-choice-list))

  (bind-key "C-x C-c" 'system-exit-menu)
  )
