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
  (("C-c SPC" . 'my/consult-launch-app)
   ("C-c r" . 'exwm-reset)	
   ("s-&" . (lambda () (interactive) (exwm-workspace-switch-create 0)))
   
   ;; workspace change for french keyboard
   ("s-é" . (lambda () (interactive) (exwm-workspace-switch-create 1)))
   ("s-\"" . (lambda () (interactive) (exwm-workspace-switch-create 2)))
   ("s-'" . (lambda () (interactive) (exwm-workspace-switch-create 3)))
   ("s-(" . (lambda () (interactive) (exwm-workspace-switch-create 4)))
   ("s--" . (lambda () (interactive) (exwm-workspace-switch-create 5)))
   ("s-è" . (lambda () (interactive) (exwm-workspace-switch-create 6)))
   ("s-_" . (lambda () (interactive) (exwm-workspace-switch-create 7)))
   ("s-ç" . (lambda () (interactive) (exwm-workspace-switch-create 8)))
   ("s-à" . (lambda () (interactive) (exwm-workspace-switch-create 9)))
   :map exwm-mode-map
   ("s-é" . (lambda () (interactive) (exwm-workspace-switch-create 1)))
   ("s-\"" . (lambda () (interactive) (exwm-workspace-switch-create 2)))
   ("s-'" . (lambda () (interactive) (exwm-workspace-switch-create 3)))
   ("s-(" . (lambda () (interactive) (exwm-workspace-switch-create 4)))
   ("s--" . (lambda () (interactive) (exwm-workspace-switch-create 5)))
   ("s-è" . (lambda () (interactive) (exwm-workspace-switch-create 6)))
   ("s-_" . (lambda () (interactive) (exwm-workspace-switch-create 7)))
   ("s-ç" . (lambda () (interactive) (exwm-workspace-switch-create 8)))
   ("s-à" . (lambda () (interactive) (exwm-workspace-switch-create 9)))
   )
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

  ;; support for multi-monitor (need to activate monitors with something like arandr)
  ;; then run my/exwm-randr-update-workspaces
  (require 'exwm-randr)
  (exwm-randr-mode 1)
  
  (defun my/exwm-randr-update-workspaces ()
    "automatically update exwm-randr-workspace-monitor-plist
from active screens."
    (interactive)
    (let* ((monitors
            (split-string
             (shell-command-to-string
              "xrandr --listactivemonitors | tail -n +2 | awk '{print $NF}'")
             "\n" t))
           (workspace 0)
           (plist nil))
      (dolist (monitor monitors)
	(setq plist
              (append plist
                      (list workspace monitor)))
	(setq workspace (1+ workspace)))
      (setq exwm-randr-workspace-monitor-plist plist)
      (message "EXWM RANDR updated : %S"
               exwm-randr-workspace-monitor-plist)
    (exwm-randr-refresh)))
  :hook
  ('exwm-update-class-hook . (lambda () (exwm-workspace-rename-buffer
					 exwm-class-name)))
  ('exwxm-update-title-hook . (lambda ()
				(when (not exwm-instance-name)
				  (exwm-workspace-rename-buffer exwm-title))
				))
  ('my/exwm-randr-update-workspaces . 'my/set-borders-and-padding)
  )
      
;; Install lemon (system monitor in echo area)
(use-package lemon
  :config
  (require 'lemon-time)
  (require 'lemon-battery)
  (require 'lemon-cpu)
  (require 'lemon-memory)
  (require 'lemon-network)
  
(defclass my/lemon-workspace-monitor (lemon-monitor)
  ((index :initform "WS "
	   :initarg :index)))

(cl-defmethod lemon-monitor-fetch ((this my/lemon-workspace-monitor))
  (if (boundp 'exwm-workspace-current-index)
      (format "[%d]" (1+ exwm-workspace-current-index))
    "[?]"))
(cl-defmethod lemon-monitor-display ((this my/lemon-workspace-monitor))
  "Default display method for Lemon monitors."
  (with-slots (display-opts) this
    (let ((val (lemon-monitor-value this))
          (index (or (plist-get display-opts :index) ""))
          (unit (or (plist-get display-opts :unit) "")))
      (concat index
              (if (not (stringp val)) "N/A"
                (format "%s%s" val unit))))))

(setq lemon-delay 0.2
      lemon-refresh-rate 1
	;; to display graphics
	lemon-sparkline-use-xpm 1
	lemon-monitors
	'(((lemon-time :display-opts '(:format " %d %b %H:%M  │ "))
	   (lemon-battery)
	   (my/lemon-workspace-monitor :display-opts '(:index " |  "))
	   (lemon-cpu-linux :display-opts '(:sparkline (:type plain :width 200) :index " │  ")
			    )
	   (lemon-memory-linux :display-opts '(:sparkline (:type plain :width 200) :index " │  ")
			       )
	   ;; (lemon-linux-network-rx :display-opts '(:sparkline (:type gridded)))
	   ;; (lemon-linux-network-tx :display-opts '(:sparkline (:type gridded)))
	   ))
	)
  (custom-set-faces
   '(lemon-battery-low-face ((t (:inherit outline-3)))))
  (set-face-foreground 'lemon-time-face nil) ; so it will be like
					; default
  (lemon-mode 1)
  )



