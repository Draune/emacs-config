;; mark
(setq my/exwm-mark nil)
(defun my/exwm-toggle-mark () (interactive)
       (if my/exwm-mark
	   (setq my/exwm-mark nil)
	 (setq my/exwm-mark t)
	 )
       (message (if my/exwm-mark "EXWM Mark set" "EXWM Mark off"))
       )
(defun my/exwm-mark-off (&rest args)
       (if my/exwm-mark
	   (progn
	     (setq my/exwm-mark nil)
	     (message "EXWM Mark off")
	     ))
       )
(keymap-set exwm-mode-map "M-SPC" 'my/exwm-toggle-mark)
(advice-add 'other-window :after #'my/exwm-mark-off)
(advice-add 'switch-to-buffer :after #'my/exwm-mark-off)
(advice-add 'kill-current-buffer :after #'my/exwm-mark-off)
(advice-add 'keyboard-quit :before #'my/exwm-mark-off)
;; movements
(keymap-set exwm-mode-map "C-n" (lambda () (interactive)
				  (if my/exwm-mark
				      (exwm-input--fake-key 'S-down)		      
				    (exwm-input--fake-key 'down)
				    )
				  ))
(keymap-set exwm-mode-map "C-p" (lambda () (interactive)
				  (if my/exwm-mark
				      (exwm-input--fake-key 'S-up)		      
				    (exwm-input--fake-key 'up)
				    )
				  ))
(keymap-set exwm-mode-map "C-b" (lambda () (interactive)
				  (if my/exwm-mark
				      (exwm-input--fake-key 'S-left)		      
				    (exwm-input--fake-key 'left)
				    )
				  ))
(keymap-set exwm-mode-map "C-f" (lambda () (interactive)
				  (if my/exwm-mark
				      (exwm-input--fake-key 'S-right)		      
				    (exwm-input--fake-key 'right)
				    )
				  ))
(keymap-set exwm-mode-map "M-n" (lambda () (interactive) (exwm-input--fake-key 'C-down)))
(keymap-set exwm-mode-map "M-p" (lambda () (interactive) (exwm-input--fake-key 'C-up)))
(keymap-set exwm-mode-map "M-b" (lambda () (interactive)
				  (if my/exwm-mark
				      (exwm-input--fake-key 'C-S-left)		      
				    (exwm-input--fake-key 'C-left)
				    )
				  ))
(keymap-set exwm-mode-map "M-f" (lambda () (interactive)
				  (if my/exwm-mark
				      (exwm-input--fake-key 'C-S-right)		      
				    (exwm-input--fake-key 'C-right)
				    )
				  ))
(keymap-set exwm-mode-map "C-a" (lambda () (interactive)
				  (if my/exwm-mark
				      (exwm-input--fake-key 'S-home)		      
				    (exwm-input--fake-key 'home)
				    )
				  ))
(keymap-set exwm-mode-map "C-e" (lambda () (interactive)
				  (if my/exwm-mark
				      (exwm-input--fake-key 'S-end)		      
				    (exwm-input--fake-key 'end)
				    )
				  ))
(keymap-set exwm-mode-map "M-v" (lambda () (interactive) (exwm-input--fake-key 'prior)))
(keymap-set exwm-mode-map "C-v" (lambda () (interactive) (exwm-input--fake-key 'next)))
;; kill-ring
(if (executable-find "xclip")
    (progn      
	(defun my/exwm-kill-ring-save () (interactive)
	       (let ((clip (shell-command-to-string "xclip -o")))
		 (when (and clip
			    (not (member clip kill-ring))) ;; dodge doubles
		   (kill-new clip)
		   )))
	(keymap-set exwm-mode-map "C-k" (lambda () (interactive) (exwm-input--fake-key 'home)
					  (exwm-input--fake-key 'S-end)
					  (my/exwm-kill-ring-save)
					  (exwm-input--fake-key 'delete)))
	(keymap-set exwm-mode-map "C-w" (lambda () (interactive) (my/exwm-kill-ring-save)
					  (exwm-input--fake-key 'delete)))
	(keymap-set exwm-mode-map "M-w" (lambda () (interactive) (my/exwm-kill-ring-save)))
	)
  ;; else
      (progn      
	(keymap-set exwm-mode-map "C-k" (lambda () (interactive) (exwm-input--fake-key 'home)
					  (exwm-input--fake-key 'S-end)
					  (exwm-input--fake-key 'C-x)))
	(keymap-set exwm-mode-map "C-w" (lambda () (interactive) (exwm-input--fake-key 'C-x)))
	(keymap-set exwm-mode-map "M-w" (lambda () (interactive) (exwm-input--fake-key 'C-c)))
	))
(keymap-set exwm-mode-map "C-y" (lambda () (interactive) (exwm-input--fake-key 'C-v)))
(defun my/exwm-paste-string (str)
  (kill-new str)
  (exwm-input--fake-key 'C-v)
  )
(defun my/exwm-yank-pop ()
  (interactive)
  (let ((str (consult--read
                kill-ring
                :prompt "Yank from kill-ring: "
                :sort nil
                :require-match t)))
    (my/exwm-paste-string str)
    ))
(keymap-set exwm-mode-map "M-y" (lambda () (interactive) (my/exwm-yank-pop)))
;; delete
(keymap-set exwm-mode-map "C-d" (lambda () (interactive) (exwm-input--fake-key 'delete)))
(keymap-set exwm-mode-map "M-d" (lambda () (interactive) (exwm-input--fake-key 'C-delete)))
;; insert
(bind-key "C-c i i" (lambda () (interactive) (exwm-input--fake-key '<)))
(bind-key "C-c i s" (lambda () (interactive) (exwm-input--fake-key '>)))
