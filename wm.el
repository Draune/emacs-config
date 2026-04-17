(use-package exwm
  :if (or
       ;; this one for the NixOS EXWM launcher
       (string-match "^[a-z0-9]\\{15\\}$" (my/emacs-parent-name))
       (equal (my/emacs-parent-name) "sddm-helper")
       (equal (my/emacs-parent-name) "ly-dm")
       (equal (my/emacs-parent-name) "xinit")
       (equal (my/emacs-parent-name) "xephyr-exwm")
       (equal (my/emacs-parent-name) "startexwm")
       )
  :bind
  ("C-c SPC" . 'my/consult-launch-app)
  ("C-c r" . 'exwm-reset)	
  :config
  (setq exwm-workspace-number 4
	;; Special EXWM bindings
	exwm-input-global-keys	'(
				  ([?\C-q] . exwm-input-send-next-key)
				  ([?\s- ] . my/consult-launch-app)
				  )
	)
  ;; Make class name the buffer name.
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
      ;; To use devil when working with X windows (like ", x o")
  (push ?, exwm-input-prefix-keys)
  ;; Utilities
  (load-relative "./wm_util.el")
  ;; Enable EXWM
  (exwm-wm-mode)
  (load-relative "./wm_key.el")
  :hook
  ('exwm-update-class-hook . (lambda () (exwm-workspace-rename-buffer exwm-class-name)))
  )
      
;; Install lemon (system monitor in echo area)
(use-package lemon
  :config
  (require 'lemon-time)
  (require 'lemon-battery)
  (require 'lemon-cpu)
  (require 'lemon-memory)
  (require 'lemon-network)
  (setq lemon-delay 0.2
	lemon-update-interval 2
	;; to display graphics
	lemon-sparkline-use-xpm 1
	lemon-monitors
	'(((lemon-time :display-opts '(:format "%d %b %H:%M"))
	   (lemon-battery)
	   (lemon-cpu-linux :display-opts '(:sparkline (:type gridded)))
	   (lemon-memory-linux :display-opts '(:sparkline (:type gridded)))
	   (lemon-linux-network-rx :display-opts '(:sparkline (:type gridded)))
	   (lemon-linux-network-tx :display-opts '(:sparkline (:type gridded)))
	   ))
	)
  (lemon-mode 1)
  )


