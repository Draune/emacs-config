;; Do not defer packages used for the notifications
;; to test notifications:
;; (alert "alert" :title "alert test")

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
  
  (require 'notifications)
  (defun org-timer-set-timer-with-title--run-countdown-timer (secs title)
  "Start countdown timer that will last SECS.
TITLE will be used by the notification displayed when time is up."
  (let ((msg (format "Start: %s (%s)"
		     (current-time-string)
		     (org-timer-secs-to-hms secs))))
    (setq org-timer-set-timer-with-title--title title)
    (setq org-timer-set-timer-with-title--msg msg)
    (run-with-timer
     secs nil (lambda ()
		(setq org-timer-countdown-timer nil
		      org-timer-start-time nil)
		(notifications-notify
		 :title org-timer-set-timer-with-title--title
		 :body org-timer-set-timer-with-title--msg
		 :app-name "Emacs"
                 :actions '("default" "default"))
		(org-timer-set-mode-line 'off)
		(run-hooks 'org-timer-done-hook)))))
  
  (defun org-timer-set-timer-with-title (&optional opt title)
    "Prompt for a duration in minutes or hh:mm:ss and set a timer that will
    notify the user with a chosen title.

If `org-timer-default-timer' is not \"0\", suggest this value as
the default duration for the timer.  If a timer is already set,
prompt the user if she wants to replace it.

Called with a numeric prefix argument OPT, use this numeric value as
the duration of the timer in minutes.

Called with a \\[universal-argument] prefix argument OPT, use
`org-timer-default-timer' without prompting the user for a duration.

With two \\[universal-argument] prefix arguments OPT, use
`org-timer-default-timer' without prompting the user for a duration
and automatically replace any running timer.

By default, the timer duration will be set to the number of
minutes in the Effort property, if any.  You can ignore this by
using three \\[universal-argument] prefix arguments."
    (interactive "P
sTimer title:")
    (when (and org-timer-start-time
	       (not org-timer-countdown-timer))
      (user-error "Relative timer is running.  Stop first"))
    (let* ((default-timer
	    ;; `org-timer-default-timer' used to be a number, don't choke:
	    (if (numberp org-timer-default-timer)
		(number-to-string org-timer-default-timer)
	      org-timer-default-timer))
	   (effort-minutes
            (cond ((derived-mode-p 'org-agenda-mode)
                   (org-get-at-bol 'effort-minutes))
                  ((derived-mode-p 'org-mode)
                   (let ((effort (org-entry-get nil org-effort-property)))
	             (when (org-string-nw-p effort)
	               (floor (org-duration-to-minutes effort)))))
                  (t nil)))
	   (minutes (or (and (numberp opt) (number-to-string opt))
			(and (not (equal opt '(64)))
			     effort-minutes
			     (number-to-string effort-minutes))
			(and (consp opt) default-timer)
			(and (stringp opt) opt)
			(read-from-minibuffer
			 "How much time left? (minutes or h:mm:ss) "
			 (and (not (string-equal default-timer "0"))
			      default-timer)))))
      (when (string-match "\\`[0-9]+\\'" minutes)
	(setq minutes (concat minutes ":00")))
      (if (not (string-match "[0-9]+" minutes))
	  (org-timer-show-remaining-time)
	(let ((secs (org-timer-hms-to-secs (org-timer-fix-incomplete minutes))))
	  (if (and org-timer-countdown-timer
		   (not (or (equal opt '(16))
			    (y-or-n-p "Replace current timer? "))))
	      (message "No timer set")
	    (when (timerp org-timer-countdown-timer)
	      (cancel-timer org-timer-countdown-timer))
	    (setq org-timer-countdown-timer-title
		  title)
	    (setq org-timer-countdown-timer
		  (org-timer-set-timer-with-title--run-countdown-timer
		   secs title))
	    (run-hooks 'org-timer-set-hook)
	    (setq org-timer-start-time (time-add nil secs))
	    (setq org-timer-pause-time nil)
	    (org-timer-set-mode-line 'on))))))
  )
