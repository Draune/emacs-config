(defcustom my/org-default-note-dir "~/Documents/notes/"
  "Default note dir for agenda and denote between everything else")
(defcustom my/org-agenda-files (list (concat my/org-default-note-dir
					     "agenda.org"))
  "Path to the default org-agenda files to reset it after using
    project-org-agenda.")
(defcustom project-org-default-note-dir "notes/"
  "Directory where notes for project-org-agenda and project-denote will be
  stored.")
(defcustom project-org-agenda-file (concat project-org-default-note-dir
					   "agenda.org")
  "Directory where notes for project-org-agenda will be stored.")
  
(use-package org
  :defer t 
  :config
  (setq org-pretty-entities t
	;; org-hide-emphasis-markers t
	org-agenda-window-setup 'current-window
	org-log-done t
	)
  
  ;; ;; Resize Org headings
  ;; (dolist (face '((org-level-1 . 1.2)
  ;;                 (org-level-2 . 1.15)
  ;;                 (org-level-3 . 1.1)
  ;;                 ;; (org-level-4 . 1.1)
  ;;                 ;; (org-level-5 . 1.1)
  ;;                 ;; (org-level-6 . 1.1)
  ;;                 ;; (org-level-7 . 1.1)
  ;;                 ;; (org-level-8 . 1.1)
  ;; 		  ))
  ;;   (set-face-attribute (car face) nil :weight 'bold :height (cdr face)))

  ;; Make the document title a bit bigger
  (set-face-attribute 'org-document-title nil :weight
		      'bold :height 1.8)
  (setq org-agenda-start-on-weekday nil)

  (setq org-todo-keywords
	'((sequence "TODO(t)" "FEEDBACK(f)" "DELEGATED(z)" "|" "DONE(d)")
	  (sequence "UNKNOWN(u)" "MAYBE(m)" "|" "VERIFIED(v)")
	  (sequence "|" "CANCELED(c)")))
  
  (defun default-org-agenda () (interactive)
	       (setq org-agenda-files my/org-agenda-files)
	       (org-agenda nil "n")
	       (setq default-directory my/org-default-note-dir) 
	       )

  (defun project-org-agenda ()
    "Start org-agenda with the project directory as org-agenda-files."
    (interactive)
    (let ((project-agenda-file (concat (project-root (project-current t))
				       project-org-agenda-file)))
      (setq org-agenda-files (list project-agenda-file))
      (if (file-exists-p project-agenda-file)
	  (progn
	    (org-agenda nil "n")
	    (setq default-directory (concat (project-root (project-current t))
					    project-org-default-note-dir)) 
	    )
	(when (yes-or-no-p (concat "Create file " project-agenda-file "?"))
	  (make-empty-file project-agenda-file t)
	  (org-agenda nil "n")
	    (setq default-directory (concat (project-root (project-current t))
					    project-org-default-note-dir)) 
	  )
	)
      )
    )
  (add-to-list 'project-switch-commands '(project-org-agenda "Agenda"))
  :bind
  (("C-c a" . default-org-agenda)
  ("C-c l s" . 'org-store-link)
  ("C-c l i" . 'org-id-store-link)
  :map project-prefix-map
  ("a" . project-org-agenda))
  )

(use-package denote
  :ensure t
  :hook (dired-mode . denote-dired-mode)
  :bind
  (("C-c n n" . denote)
   ("C-c n r" . denote-rename-file)
   ("C-c n l" . denote-link)
   ("C-c n b" . denote-backlinks)
   ("C-c n d" . denote-dired)
   ("C-c n g" . denote-grep)
   ("C-c n r" . default-denote)
   :map project-prefix-map
   ("n" . project-denote))
  :config
  (setq denote-directory (expand-file-name my/org-default-note-dir))

  (defun default-denote ()
    "Reset denote dir to my/org-default-note-dir"
    (interactive)
    (setq denote-directory (expand-file-name
			    my/org-default-note-dir)))
  
  (defun project-denote ()
    "Change denote-directory to the project directory."
    (interactive)
    (let ((project-denote-dir (concat (project-root (project-current t))
				       project-org-default-note-dir)))
      (if (file-exists-p project-denote-dir)
	  (setq denote-directory (expand-file-name project-denote-dir))
	(when (yes-or-no-p (concat "Create dir " project-denote-dir "?"))
	  (make-directory project-denote-dir)
	  (setq denote-directory (expand-file-name project-denote-dir))
	  )
	)
      )
    )
  (add-to-list 'project-switch-commands '(project-denote "Change denote dir"))
  
  ;; Automatically rename Denote buffers when opening them so that
  ;; instead of their long file name they have, for example, a literal
  ;; "[D]" followed by the file's title.  Read the docstring of
  ;; `denote-rename-buffer-format' for how to modify this.
  (denote-rename-buffer-mode 1))
