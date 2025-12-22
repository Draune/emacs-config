;; Install exwm (just if Emacs was called by a display manager/custom script/or xinit)
(if (or
     (equal (my/emacs-parent-name) "sddm-helper")
     (equal (my/emacs-parent-name) "ly-dm")
     (equal (my/emacs-parent-name) "xinit")
     (equal (my/emacs-parent-name) "xephyr-exwm")
     )
    (progn
      (use-package exwm
	:ensure t
	:config
	(setq exwm-workspace-number 4)
	;; Make class name the buffer name.
	(add-hook 'exwm-update-class-hook
		  (lambda () (exwm-workspace-rename-buffer exwm-class-name)))

	;; Lauch app
	(defun my/consult-launch-app ()
	  "Consult launch app for EXWM"
	  (interactive)
	  (let ((command (consult--read
			  my/exec-list
			  :prompt "Launch App: "
			  :history 'shell-command-history
			  :category 'command)))
	    (start-process-shell-command command nil command)))
	  
	(bind-key "s-SPC" 'my/consult-launch-app)

	(bind-key "s-r" 'exwm-reset)	
	
	;; Special EXWM bindings
	(setq exwm-input-global-keys
	      '(
		([?\C-q] . exwm-input-send-next-key)
		([?\s- ] . my/consult-launch-app)
		))
	;; To use devil when working with X windows (like ", x o")
	(push ?, exwm-input-prefix-keys)
	;; To get Emacs bindings inside X windows (don't works with devil, ie. ", n" will not work but "C-n" will)
	(setq exwm-input-simulation-keys
	      '(([?\C-b] . [left])
		([?\C-f] . [right])
		([?\C-p] . [up])
		([?\C-n] . [down])
		([?\M-b] . [C-left])
		([?\M-f] . [C-right])
		([?\M-p] . [C-up])
		([?\M-n] . [C-down])
		([?\C-a] . [home])
		([?\C-e] . [end])
		([?\M-v] . [prior])
		([?\C-v] . [next])
		([?\C-d] . [delete])
		([?\C-k] . [S-end delete])
		([?\C-w] . [?\C-x])
		([?\M-w] . [?\C-c])
		([?\C-y] . [?\C-v])
		([?\M-d] . [C-delete])
		([?\;] . [?,])))

	;; Utilities (if it is launched as a standalone WM,i.e. not launched with Xephyr)
	(if (not (equal (my/emacs-parent-name) "xephyr-exwm"))
	    (progn
	      (load-file "~/.emacs.d/wm_util.el")
	      (nconc exwm-input-global-keys
		     '(
		       ([XF86MonBrightnessDown] . brightness_dec)
		       ([XF86MonBrightnessUp] . brightness_inc)
		       ([XF86AudioLowerVolume] . sound_volume_dec)
		       ([XF86AudioRaiseVolume] . sound_volume_dec)
		       ([XF86AudioMute] . sound_mute_toggle)
		       ([print] . screenshot)
		       ))
	      ))
	;; Enable EXWM
	(exwm-wm-mode)
	)
      
      ;; Install lemon (system monitor in echo area)
      (use-package lemon
	:demand t
	:config
	(setq lemon-delay 0.2)
	(setq lemon-update-interval 2)
	;; to display graphics
	(setq lemon-sparkline-use-xpm 1)
	(setq lemon-monitors
	      '(((lemon-time :display-opts '(:format "%d %b %H:%M"))
		 (custom-set-faces
		  '(lemon-time-face ((t (:foreground "#bd00ff")))))
		 (lemon-battery)
		 (custom-set-faces
		  '(lemon-battery-low-face ((t (:background "#ee27eb"))))
		  '(lemon-battery-medium-face ((t (:foreground "#ffd400"))))
		  '(lemon-battery-charging-face ((t (:foreground "#ee27eb"))))
		  '(lemon-battery-full-face ((t (:foreground "#ee27eb"))))
		  )
		 (lemon-cpu-linux :display-opts '(:sparkline (:type gridded)))
		 (lemon-memory-linux :display-opts '(:sparkline (:type gridded)))
		 (lemon-linux-network-rx :display-opts '(:sparkline (:type gridded)))
		 (lemon-linux-network-tx :display-opts '(:sparkline (:type gridded)))
		 )))

	(lemon-mode 1))
      ))


