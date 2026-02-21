;; Install exwm (just if Emacs was called by a display manager/custom script/or xinit)
(if (or
     ;; this one for the NixOS EXWM launcher
     (string-match "^[a-z0-9]\\{15\\}$" (my/emacs-parent-name))
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
	;; Utilities (if it is launched as a standalone WM,i.e. not launched with Xephyr)
	(if (not (equal (my/emacs-parent-name) "xephyr-exwm"))
	    (progn
	      (load-relative "./wm_util.el")
	      ))
	;; Enable EXWM
	(exwm-wm-mode)
	(load-relative "./wm_key.el")
	)
      
      ;; Install lemon (system monitor in echo area)
      (use-package lemon
	:ensure t
	:vc (:url "https://codeberg.org/emacs-weirdware/lemon")
	:config
	(setq lemon-delay 0.2)
	(setq lemon-update-interval 2)
	;; to display graphics
	(setq lemon-sparkline-use-xpm 1)
	(setq lemon-monitors
	      '(((lemon-time :display-opts '(:format "%d %b %H:%M"))
		 (lemon-battery)
		 (lemon-cpu-linux :display-opts '(:sparkline (:type gridded)))
		 (lemon-memory-linux :display-opts '(:sparkline (:type gridded)))
		 (lemon-linux-network-rx :display-opts '(:sparkline (:type gridded)))
		 (lemon-linux-network-tx :display-opts '(:sparkline (:type gridded)))
		 )))

	(lemon-mode 1))
      ))


