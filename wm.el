;; Install exwm (just if Emacs was called by xinit)
(if (equal (emacs-parent-name) "xinit")
    (progn
      (use-package exwm
	     :ensure t
	     :config
	     (setq exwm-workspace-number 4)
	     ;; Make class name the buffer name.
	     (add-hook 'exwm-update-class-hook
		       (lambda () (exwm-workspace-rename-buffer exwm-class-name)))
	     ;; Special EXWM bindings
	     (setq exwm-input-global-keys
		   '(
		     ([?\C-q] . exwm-input-send-next-key)
		     ))
	     ;; To use devil when working with X windows (like ", x o")
	     (push ?, exwm-input-prefix-keys)
	     ;; To get Emacs bindings inside X windows (don't works with devil, ie. ", n" will not work but "C-n" will)
	     (setq exwm-input-simulation-keys
		   '(([?\C-b] . [left])
		     ([?\C-f] . [right])
		     ([?\C-p] . [up])
		     ([?\C-n] . [down])
		     ([?\C-a] . [home])
		     ([?\C-e] . [end])
		     ([?\M-v] . [prior])
		     ([?\C-v] . [next])
		     ([?\C-d] . [delete])
		     ([?\C-k] . [S-end delete])
		     ([?\C-w] . [?\C-x])
		     ([?\M-w] . [?\C-c])
		     ([?\C-y] . [?\C-v])
		     ([?\M-d] . [C-delete])))
	     ;; Lauch app
	     (bind-key "C-c r" 'exwm-reset)
	     (bind-key "C-c a" (lambda (cmd)
				  (interactive (list (read-shell-command "$ ")))
				  (start-process-shell-command cmd nil cmd)))
	     ;; Enable EXWM
	     (exwm-wm-mode)
	     )
	   
	   ;; Install lemon (system monitor in echo area)
	   (use-package lemon
	     :ensure t
	     :vc (:url "https://codeberg.org/emacs-weirdware/lemon.git"
		       :rev :newest)
	     :config
	     (setq lemon-delay 0.2)
	     (setq lemon-update-interval 2)
	     ;; to display graphics
	     (setq lemon-sparkline-use-xpm 1)
	     (setq lemon-monitors
		   '(((lemon-time :display-opts '(:format "%H:%M"))
		      (lemon-battery)
		      (lemon-cpu-linux :display-opts '(:sparkline (:type gridded)))
		      (lemon-memory-linux :display-opts '(:sparkline (:type gridded)))
		      (lemon-linux-network-rx :display-opts '(:sparkline (:type gridded)))
		      (lemon-linux-network-tx :display-opts '(:sparkline (:type gridded)))
		      )))

	     (lemon-mode 1))
	   
	   ;; Screenshots
	   (if (executable-find "maim")
	       (progn
		 (bind-key "<print>" (lambda () (interactive) (shell-command (format-time-string "maim '/home/louis/Pictures/%F_%X.png'"))))
		 (bind-key "C-<print>" (lambda () (interactive) (shell-command (format-time-string "maim -s '/home/louis/Pictures/%F_%X.png'"))))
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
		   )
		 (bind-key "<XF86MonBrightnessDown>" (lambda () (interactive) (brightness_add -0.05)))
		 (bind-key "<XF86MonBrightnessUp>" (lambda () (interactive) (brightness_add 0.05)))
		 ))
  ))


