(defcustom org-default-note-dir "~/Documents/notes/"
  "Default note dir for agenda and denote between everything else")
(defcustom org-default-agenda-files (list (concat org-default-note-dir
					     "agenda.org"))
  "Path to the default org-agenda files to reset it after using
    project-org-agenda.")
(defcustom project-org-default-note-dir "notes/"
  "Directory where notes for project-org-agenda and project-denote will be
  stored.")
(defcustom project-org-agenda-file (concat project-org-default-note-dir
					   "agenda.org")
  "Directory where notes for project-org-agenda will be stored.")

(defun project-get-all-note-directories ()
  "Allow to get all note directories inside projects according
project-org-default-note-dir (return only existing directories)."
  (map-filter
   (lambda (key val) (file-exists-p key))
    (mapcar
     (lambda (dir) (list (concat (car dir) project-org-default-note-dir)))
     project--list)))

(defun project-get-all-agenda-files ()
  "Get all existing agenda files stocked in project directories according to
project-org-agenda-file"
  (map-filter
   (lambda (key val) (file-exists-p key))
    (mapcar
     (lambda (dir) (list (concat (car dir) project-org-agenda-file)))
     project--list)))

(defun get-all-note-directories ()
  "Get all note directories: directories in projects according to
project-org-default-note-dir and default note directory (org-default-note-dir)"
  (if (file-exists-p org-default-note-dir)
      (cons (list org-default-note-dir)
	    (project-get-all-note-directories))
    (project-get-all-note-directories)))

(defun get-all-agenda-files ()
  "Return a flatten list of all agenda files: files in projects according to
project-org-agenda-file and default agenda files (org-default-agenda-files)"
  (flatten-tree
   (cons org-default-agenda-files
	 (project-get-all-agenda-files))))
  
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
	'((sequence "TODO(t)" "PLANNED(p)" "FEEDBACK(f)" "DELEGATED(z)" "|" "DONE(d)")
	  (sequence "UNKNOWN(u)" "MAYBE(m)" "|" "VERIFIED(v)")
	  (sequence "|" "CANCELED(c)")))
  
  (defun default-org-agenda () (interactive)
	       (setq org-agenda-files org-default-agenda-files)
	       (org-agenda nil "n")
	       (setq default-directory org-default-note-dir) 
	       )

  (require 'project)
  (defun project-org-agenda ()
    "Start org-agenda with the project directory as org-agenda-files."
    (interactive)
    (let ((project-directory (project-root (project-current t))))
      (let ((project-agenda-file (concat project-directory
					 project-org-agenda-file)))
	(setq org-agenda-files (list project-agenda-file))
	(if (file-exists-p project-agenda-file)
	    (progn
	      (org-agenda nil "n")
	      (setq default-directory (concat project-directory
					      project-org-default-note-dir)) 
	      )
	  (when (yes-or-no-p (concat "Create file " project-agenda-file "?"))
	    (make-empty-file project-agenda-file t)
	    (org-agenda nil "n")
	    (setq default-directory (concat project-directory
					    project-org-default-note-dir)) 
	    )
	  )
	)
      )
    )

  (defun global-org-agenda ()
    (interactive)
    (setq org-agenda-files (get-all-agenda-files))
    (org-agenda nil "n")
    (setq default-directory org-default-note-dir))
  
  (add-to-list 'project-switch-commands '(project-org-agenda "Agenda"))
  :bind
  (("C-c a" . 'default-org-agenda)
  ("C-c l s" . 'org-store-link)
  ("C-c l i" . 'org-id-store-link)
  ("C-c g a" . 'global-org-agenda)
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
  (setq denote-known-keywords '())
  (setq denote-directory (expand-file-name org-default-note-dir))

  (defun default-denote ()
    "Reset denote dir to org-default-note-dir"
    (interactive)
    (setq denote-directory (expand-file-name
			    org-default-note-dir)))
  
  (require 'project)
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
