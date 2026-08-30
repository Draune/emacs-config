(defun my/emacs-ppid ()
  "Retourne le PID (string) du processus parent d’Emacs."  
  (string-trim
   (shell-command-to-string
    (format "ps -o ppid= -p %d" (emacs-pid)))))

(defun my/emacs-parent-name ()
  "Retourne le nom du processus parent d’Emacs (Linux uniquement)."
  (string-trim
   (shell-command-to-string
    (format "ps -o comm= -p %s" (my/emacs-ppid)))))

(defun char-choice-menu (choice-list &optional menu-prompt)
  "Allow to create a choice menu from a list of choices, a choice is of the
form: '(KEY PROMPT-NAME FUNCTION)

For exemple the list of choice:
'((\"e\" \"Exit\" (lambda () (message \"Exit\")))
  (\"r\" \"Reboot\" (lambda () (message \"Reboot\"))))
Will prompt : \"Choose: Exit(e) Reboot(r)\"
Pressing \"e\" will result of the execution of (lambda () (message \"Exit\"))"
  (let ((prompt
	 (concat (if (stringp menu-prompt) menu-prompt "Choose: ")
		 (let (res)
		   (dolist (choice choice-list res)
		     (setq res
			   (concat res
				   (nth 1 choice)
				   "("
				   (car choice)
				   ") "))))))
	(chars
	 (let (res) (dolist (choice choice-list res)
		      (setq res (cons (car choice) res))
		      ))))
    (let ((arg (read-char-choice prompt chars)))
      ;; (message (char-to-string arg))
      (funcall (nth 2 (assoc (char-to-string arg) choice-list))))
    ))

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
				   "\\*Async Shell Command\\*"
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
