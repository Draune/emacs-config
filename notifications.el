;; Do not defer packages used for the notifications
;; to test notifications:
;; (alert "alert" :title "alert test")

(use-package popper
  :ensure t
  :bind
  ("C-c p p" . 'popper-toggle)
  ("C-c p c" . 'popper-cycle)
  ("C-c p t" . 'popper-toggle-type)
  :config
  (setq popper-echo-dispatch-keys '(?s ?d ?f ?g ?h ?j ?k ?l ?m)
	popper-reference-buffers '("\\*Messages\\*"
				   "Output\\*$"
				   "\\*Warnings\\*"
				   "\\*Backtrace\\*"
				   help-mode
				   shortdoc-mode
				   compilation-mode
				   ;; like that I can create popper buffers
				   ;; without modifying this variable
				   "popper"))
  (defun popper-buffer-buried-p (BUFF-OR-NAME)
    "Return non-nil if BUFF-OR-NAME is either buried or does not exist."
    (map-every-p (lambda (key value)
		   (not (eq value (get-buffer
				   BUFF-OR-NAME))))
		 popper-open-popup-alist))
  (popper-mode +1)
  (popper-echo-mode +1)
  )

(use-package ednc
  :ensure t
  :bind
  ("C-c d d" . ednc-dismiss-last-notification)
  ("C-c d s" . ednc-toggle-show-notifications)
  :config
  (setq ednc-log-name "*ednc-popper-log*")
  (generate-new-buffer ednc-log-name)
  
  (defun ednc-dismiss-last-notification (&optional num-arg)
    "Dismiss the last notification.
When using a prefix-arg, dismiss the nth notification, 1 being the last one."
    (interactive "p")
    (let ((notif (nth (- num-arg 1) (ednc-notifications))))
	(when notif
	  (ednc-dismiss-notification notif))))  
  
  (defun ednc-show-notifications (&rest _)
    "Show EDNC notifications"
    (unless (get-buffer ednc-log-name)
      (generate-new-buffer ednc-log-name))
    (let ((ednc-buffer (get-buffer ednc-log-name)))
      (popper-display-popup-at-bottom ednc-buffer)
      (with-current-buffer ednc-buffer
	(set-window-point (get-buffer-window ednc-buffer) (point-max)))
      )
    )
  
  (defun ednc-toggle-show-notifications ()
    "Will toggle visibility of the EDNC log buffer."
    (interactive)
    (let ((ednc-buffer (get-buffer ednc-log-name)))
      (if (popper-buffer-buried-p ednc-buffer)
	  (ednc-show-notifications)
	(popper--delete-popup (get-buffer-window ednc-buffer)))
    ))
  (add-hook 'ednc-notification-presentation-functions 'ednc-show-notifications)
  :hook
  ('after-init-hook . (lambda () (ednc-mode 1)))
  )

(use-package org-alert
  :ensure t
  :config
  (setq org-alert-interval 300
	org-alert-notify-cutoff 10
	org-alert-notify-after-event-cutoff 10
	;; so alerts will be send on dbus and displayed with ednc
	alert-default-style 'notifications)
  (org-alert-enable)
  )
