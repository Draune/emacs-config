;; Util file for EXWM that will be loaded by wm.el

;; Screenshots
(if (executable-find "maim")
    (progn
      (defun screenshot () (interactive)
	     (shell-command (format-time-string "maim -s '/home/louis/Pictures/%F_%X.png'")))
      (bind-key "<print>" 'screenshot)
      ))

(if (executable-find "xrandr")
    (progn
      (setq brightness 1.0)
      (defun brightness_add (to_add)
	"Add brightness, take account of the current brightness (no more than 1.0 or less than 0.0)"
	(setq final_brightness (+ brightness to_add))
	(if (and (<= final_brightness 1.0) (>= final_brightness 0.0))
	    (progn
	      (shell-command (format "xrandr --output eDP-1 --brightness %f" final_brightness))
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
      ))
(if (executable-find "xtrlock")
    (progn
      (require 'zone)
      (setq zone-programs [zone-pgm-whack-chars])  
      (defun lock-screen ()
	"Lock screen using (zone) and xtrlock
 calls M-x zone on all frames and runs xtrlock"
	(interactive)
	(shell-command "xtrlock")
	)
      (bind-key "s-l" 'lock-screen)
      ))

;; Volume Control (pulseaudio)
(if (executable-find "pactl")
    (progn
      (setq sound_volume 0)
      (shell-command "pactl set-sink-volume @DEFAULT_SINK@ 0%")
      (setq sound_mute nil)
      (shell-command "pactl set-sink-mute @DEFAULT_SINK@ false")
      (defun sound_volume_add (to_add)
	(setq final_sound_volume (+ sound_volume to_add))
	(if (and (<= final_sound_volume 100) (>= final_sound_volume 0))
            (progn
              (shell-command (format "pactl set-sink-volume @DEFAULT_SINK@ %d%%" final_sound_volume))
              (setq sound_volume final_sound_volume)
              ))
	(message "Volume: %d%%" sound_volume)
	)
      (defun sound_mute_toggle ()
	(interactive)
	(if sound_mute
	    (progn
	      (setq sound_mute nil)
	      (shell-command "pactl set-sink-mute @DEFAULT_SINK@ false")
	      (message "Volume: on")
	      )
	  (progn
	    (setq sound_mute t)
	    (shell-command "pactl set-sink-mute @DEFAULT_SINK@ true")
	    (message "Volume: mute")
	    )
	  )
	)
      (defun sound_volume_inc () (interactive)
	     (sound_volume_add 5))
      (defun sound_volume_dec () (interactive)
	     (sound_volume_add -5))
      (bind-key "<XF86AudioLowerVolume>" 'sound_volume_dec)
      (bind-key "<XF86AudioRaiseVolume>" 'sound_volume_inc)
      (bind-key "<XF86AudioMute>" 'sound_mute_toggle)
      ))
