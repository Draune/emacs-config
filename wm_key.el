;; movements
(keymap-set exwm-mode-map "C-n" (lambda () (interactive) (exwm-input--fake-key 'down)))
(keymap-set exwm-mode-map "C-p" (lambda () (interactive) (exwm-input--fake-key 'up)))
(keymap-set exwm-mode-map "C-b" (lambda () (interactive) (exwm-input--fake-key 'left)))
(keymap-set exwm-mode-map "C-f" (lambda () (interactive) (exwm-input--fake-key 'right)))
(keymap-set exwm-mode-map "M-n" (lambda () (interactive) (exwm-input--fake-key 'C-down)))
(keymap-set exwm-mode-map "M-p" (lambda () (interactive) (exwm-input--fake-key 'C-up)))
(keymap-set exwm-mode-map "M-b" (lambda () (interactive) (exwm-input--fake-key 'C-left)))
(keymap-set exwm-mode-map "M-f" (lambda () (interactive) (exwm-input--fake-key 'C-right)))
(keymap-set exwm-mode-map "C-a" (lambda () (interactive) (exwm-input--fake-key 'home)))
(keymap-set exwm-mode-map "C-e" (lambda () (interactive) (exwm-input--fake-key 'end)))
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
