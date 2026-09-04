;; Here is all the configs directly linked to configuring emacs defaults (and
;; the theme)
(use-package emacs
  :init
  ;; Disable bad GUI from Emacs
  (when (display-graphic-p)
    (menu-bar-mode -1)
    (scroll-bar-mode -1)
    (tool-bar-mode -1)
    )

  (setq inhibit-startup-screen 1)
  
  ;; (global-display-line-numbers-mode)

  ;; Highlight the cursor line
  (global-hl-line-mode)
  ;; Disable cursor blinking
  (blink-cursor-mode -1)
  ;; Increase font size
  (set-face-attribute 'default nil :height 130)
  ;; Delete marked region when editing (which is not activated by default)
  (delete-selection-mode 1)
  ;; Move elsewhere backups and auto-saves
  (if (eq (file-directory-p "~/.emacs.d/backups") nil)
      (make-directory "~/.emacs.d/backups"))
  (if (eq (file-directory-p "~/.emacs.d/auto-saves") nil)
      (make-directory "~/.emacs.d/auto-saves"))
  (setq backup-directory-alist `(("." . "~/.emacs.d/backups"))
	backup-by-copying t
	auto-save-file-name-transforms `((".*" "~/.emacs.d/auto-saves/" t))
	create-lockfiles nil)

  (setq completion-ignore-case t)
  ;; Deactivate the bell sounds
  (setq ring-bell-function 'ignore)
  ;; To set $PATH (for ein and eshell)
  (let ((my-path (expand-file-name "~/.local/bin")))
    (setenv "PATH" (concat (getenv "PATH") ":" my-path))
    (add-to-list 'exec-path my-path))

  (if (file-exists-p "~/.emacs.d/theme.el")
      (load "~/.emacs.d/theme.el"))

  (setq use-short-answers t)
  (setq enable-recursive-minibuffers t)
  (minibuffer-depth-indicate-mode 1)

  (defun neg-first-or-prefix-arg (oldfun &rest args)
    (let ((first-arg (car args)))
      (let
          ((arg (if first-arg
                    (if (eq first-arg 0)
			1
                      (if (eq first-arg '-)
			  -1
			first-arg)
                      )
		  (if current-prefix-arg
                      (if (eq current-prefix-arg 0)
			  1
			(if (eq current-prefix-arg '-)
                            -1
			  current-prefix-arg)
			)
                    1)
		  )))
	(apply oldfun (list (- arg))))))

  (advice-add 'upcase-word :around 'neg-first-or-prefix-arg)
  (advice-add 'downcase-word :around 'neg-first-or-prefix-arg)
  (advice-add 'capitalize-word :around 'neg-first-or-prefix-arg)
  
  (electric-pair-mode 1)
  
  ;; padding between windows
  (defun my/set-borders-and-padding ()
    (interactive)
    (modify-all-frames-parameters
     '((right-divider-width . 1)
       ;; (internal-border-width . 20)
       ))
    (dolist (face '(window-divider
                    window-divider-first-pixel
                    window-divider-last-pixel
		    ))
      (face-spec-reset-face face)
      (set-face-foreground face (face-attribute 'default :background)))
    (set-face-background 'fringe (face-attribute 'default :background))
    )
  (my/set-borders-and-padding)
  
  ;; so the kmacro repeats correctly when taping "e"
  (setq kmacro-call-repeat-key 101)
  
  ;; better pdf-resolution
  (setq doc-view-resolution 300)

  (setopt display-fill-column-indicator-character ?'
	fill-column 80)
  (global-display-fill-column-indicator-mode 1)

  (defun my/french-digit-argument (arg)
  "Part of the numeric argument for the next command.
\\[universal-argument] following digits or minus sign ends the argument."
  (interactive "P")
  (prefix-command-preserve-state)
  (let* ((char (if (integerp last-command-event)
		   last-command-event
		 (get last-command-event 'ascii-character)))
	 (digit (pcase char
		  (?à 0)
		  (?& 1)
		  (?é 2)
		  (?\" 3)
		  (?\' 4)
		  (?\( 5)
		  (?- 6)
		  (?è 7)
		  (?_ 8)
		  (?ç 9)
		  )))
    (setq prefix-arg (cond ((integerp arg)
                            (+ (* arg 10)
			       (if (< arg 0) (- digit) digit)))
                           ((eq arg '-)
                            ;; Treat -0 as just -, so that -01 will work.
                            (if (zerop digit) '- (- digit)))
                           (t
                            digit))))
  (universal-argument--mode))
  
  (defun interactive-kill-new (text) (interactive "sKill text: ")
	 (kill-new text))

  ;; to get human-readable size in dired
  (setq dired-listing-switches "-alFh")

  (require 'project)
  (setq project-vc-extra-root-markers '(".project" ".project.el" ".projectile"))
  (setq project-buffers-viewer 'project-list-buffers-ibuffer)

  (defun project-kill-project (&optional del-project)
    (interactive "P")
    (let ((pr (project-current t)))
      (let ((pr-root (project-root pr))
	    (pr-buffers (project--buffers-to-kill pr)))
	;; like `project-kill-buffers' but if I use this function it call a
	;; second time `project-current'
	(when (and pr-buffers (yes-or-no-p (format "Kill %d buffers?"
						   (length pr-buffers))))
	  (mapcar 'kill-buffer pr-buffers))
      (when del-project
	(let ((extra-root-marker
	       (map-some (lambda (key value) (if (file-exists-p key) key nil))
			 (mapcar 'list
				 (mapcar (lambda (file) (concat pr-root file))
					 project-vc-extra-root-markers)))))
	  (when (yes-or-no-p (if extra-root-marker
				 (concat "Forget project and remove "
					 extra-root-marker "?")
			       "Forget project?"))
	    (when extra-root-marker
	      (delete-file extra-root-marker))
	    (project-forget-project pr-root)))))))
  
  
  (defun project-prompt-project-dir-create (dir)
    (when (stringp dir)
      (let ((try-vc-ret (project-try-vc--search dir)))
	(unless try-vc-ret
	  (let ((project-marker (car project-vc-extra-root-markers)))
	    (when (and (stringp project-marker)
		       (yes-or-no-p (concat "Create " dir project-marker "?")))
	      (make-empty-file (concat dir project-marker)))))))
    dir)

  (advice-add 'project-prompt-project-dir :filter-return
	      'project-prompt-project-dir-create)
  
  (defvar-keymap global-command-map
    :doc "Keymap for the global commands (generally acts on defaults and project
    directories)")
  (keymap-set global-map "C-c g" global-command-map)

  :hook
  ((text-mode-hook
    prog-mode-hook
    latex-mode-hook
    org-mode-hook
    markdown-mode-hook)
   . auto-fill-mode)
  :bind
  ;; My keybindings
  (("C-c k" . 'kill-current-buffer)
  ;; Doublons here for better experience with Azerty keyboards 
  ("C-x \"" . 'split-window-right)
  ("C-x é" . 'split-window-below)
  ("C-x à" . 'delete-window)
  ("C-x &" . 'delete-other-windows)

  ;; better functions fo default keybindings
  ("C-k" . 'kill-whole-line)
  ("M-n" . (lambda () (interactive) (next-line 10)))
  ("M-p" . (lambda () (interactive) (previous-line 10)))
  ("C-o" . (lambda () (interactive)
	     (call-interactively 'move-beginning-of-line)
	     (call-interactively 'open-line)
	     (call-interactively 'indent-for-tab-command)))
  ("M-a" . 'beginning-of-buffer)
  ("M-e" . 'end-of-buffer)
  ("C-x C-b" . 'ibuffer)
  
  ;; Because I don't have ">" and "<" on my keyboard
  ("C-c i i" . (lambda () (interactive) (insert "<")))
  ("C-c i s" . (lambda () (interactive) (insert ">")))
  ("C-c r" . 'replace-string)
  
  ("M-k" . 'interactive-kill-new)
  
  ("C-&" . 'my/french-digit-argument)
  ("C-é" . 'my/french-digit-argument)
  ("C-\"" . 'my/french-digit-argument)
  ("C-'" . 'my/french-digit-argument)
  ("C-(" . 'my/french-digit-argument)
  ("C--" . 'my/french-digit-argument)
  ("C-è" . 'my/french-digit-argument)
  ("C-_" . 'my/french-digit-argument)
  ("C-ç" . 'my/french-digit-argument)
  ("C-à" . 'my/french-digit-argument)
  
  ("C-x C-b" . 'ibuffer)
  
  :map universal-argument-map
  ("&" . 'my/french-digit-argument)
  ("é" . 'my/french-digit-argument)
  ("\"" . 'my/french-digit-argument)
  ("'" . 'my/french-digit-argument)
  ("(" . 'my/french-digit-argument)
  ("-" . 'my/french-digit-argument)
  ("è" . 'my/french-digit-argument)
  ("_" . 'my/french-digit-argument)
  ("ç" . 'my/french-digit-argument)
  ("à" . 'my/french-digit-argument)

  :map project-prefix-map
  ("k" . 'project-kill-project)
  
  ;; Use M-p and M-n for the command history when using M-&
  ;; Already binded but (I don't really know why) if I don't declare it
  ;; there is a bug when repeated
  :map minibuffer-local-shell-command-map
  ("M-p" . 'previous-line-or-history-element)
  ("M-n" . 'next-line-or-history-element))
  )
