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
  (flatten-tree
   (if (file-exists-p org-default-note-dir)
       (cons org-default-note-dir
	     (project-get-all-note-directories))
     (project-get-all-note-directories))))

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
	'((sequence "TODO(t)" "PLANNED(p)" "FEEDBACK(f)" "DELEGATED(z)" "|"
	"DONE(d)")
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
  :map global-command-map
  ("a" . 'global-org-agenda)
  :map project-prefix-map
  ("a" . project-org-agenda))
  )

(defvar-keymap default-denote-map
  :doc "Keymap for the default denote commands (that act on the
  org-default-note-dir or are directory agnostic)")
(keymap-set global-map "C-c n" default-denote-map)
(defvar-keymap global-denote-map
  :doc "Keymap for the global denote commands (that act on the
  org-default-note-dir and the project note directories)")
(keymap-set global-command-map "n" global-denote-map)

(use-package denote
  :ensure t
  :defer t
  :commands
  denote            
  denote-find-file
  denote-link       
  denote-backlinks  
  denote-dired      
  denote-grep       
  default-denote
  default-denote-find-file
  default-denote-link       
  default-denote-backlinks  
  default-denote-dired      
  default-denote-grep       
  denote-rename-file
  project-denote-menu
  project-denote
  project-denote-find-file
  project-denote-link       
  project-denote-backlinks  
  project-denote-dired      
  project-denote-grep
  global-denote-find-file
  global-denote-link       
  global-denote-backlinks  
  global-denote-dired      
  global-denote-grep       
  :hook (dired-mode . denote-dired-mode)
  :bind
  (:map default-denote-map
   ("n" . default-denote)
   ("f" . default-denote-find-file)
   ("l" . default-denote-link)
   ("b" . default-denote-backlinks)
   ("d" . default-denote-dired)
   ("g" . default-denote-grep)
   ;; do not need default or project, works everywhere
   ("r" . denote-rename-file)
   :map project-prefix-map
   ("n" . project-denote-menu)
   :map global-denote-map
   ("f" . global-denote-find-file)
   ("l" . global-denote-link)
   ("b" . global-denote-backlinks)
   ("d" . global-denote-dired)
   ("g" . global-denote-grep))
  :config
  (setq denote-known-keywords '())
  (setq denote-directory (expand-file-name org-default-note-dir))
  
  (defun denote-find-file ()
    (interactive)
    (let ((denote-files (denote--directory-get-files)))
      (let ((chosen-file (completing-read
			  "Denote find file: "
			  (mapcar 'file-name-nondirectory denote-files)
			  nil
			  t)))
	(find-file (car (car (map-filter
			      (lambda (key value)
				(string-match-p
				 chosen-file
				 key 
				 ))
			      (mapcar 'list denote-files)
			      )))))))

  (defun default-denote-dir ()
    "Reset denote dir to org-default-note-dir."
    (setq denote-directory (expand-file-name
			    org-default-note-dir)))
  (defun default-denote ()
    "Call denote after resetting denote-directory to org-default-note-dir."
    (interactive)
    (default-denote-dir)
    (call-interactively 'denote))

  (defun default-denote-find-file ()
    "Call denote-find-file after resetting denote-directory to
org-default-note-dir."
    (interactive)
    (default-denote-dir)
    (call-interactively 'denote-find-file))

  (defun default-denote-link ()
    "Call denote-link after resetting denote-directory to org-default-note-dir."
    (interactive)
    (default-denote-dir)
    (call-interactively 'denote-link))

  (defun default-denote-backlinks ()
    "Call denote-backlinks after resetting denote-directory to
org-default-note-dir."
    (interactive)
    (default-denote-dir)
    (call-interactively 'denote-backlinks))

  (defun default-denote-dired ()
    "Call denote-dired after resetting denote-directory to org-default-note-dir."
    (interactive)
    (default-denote-dir)
    (call-interactively 'denote-dired))

  (defun default-denote-grep ()
    "Call denote-grep after resetting denote-directory to org-default-note-dir."
    (interactive)
    (default-denote-dir)
    (call-interactively 'denote-grep))
  
  (require 'project)
  (defun project-denote-dir ()
    "Change denote-directory to the project directory."
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
  
  (defun project-denote ()
    "Call denote after setting denote-directory to the wanted project directory
concatenated with project-org-default-note-dir." 
    (interactive)
    (project-denote-dir)
    (call-interactively 'denote))
  
  (defun project-denote-find-file ()
    "Call denote-find-file after setting denote-directory to the wanted project
directory concatenated with project-org-default-note-dir." 
    (interactive)
    (project-denote-dir)
    (call-interactively 'denote-find-file))
  
  (defun project-denote-link ()
    "Call denote-link after setting denote-directory to the wanted project
directory concatenated with project-org-default-note-dir." 
    (interactive)
    (project-denote-dir)
    (call-interactively 'denote-link))

  (defun project-denote-backlinks ()
    "Call denote-backlinks after setting denote-directory to the wanted project
directory concatenated with project-org-default-note-dir." 
    (interactive)
    (project-denote-dir)
    (call-interactively 'denote-backlinks))

  (defun project-denote-dired ()
    "Call denote-dired after setting denote-directory to the wanted project
directory concatenated with project-org-default-note-dir." 
    (interactive)
    (project-denote-dir)
    (call-interactively 'denote-dired))

  (defun project-denote-grep ()
    "Call denote-grep after setting denote-directory to the wanted project
directory concatenated with project-org-default-note-dir." 
    (interactive)
    (project-denote-dir)
    (call-interactively 'denote-grep))
  
  (defcustom project-denote-char-choice-list
    '(("n" "denote" project-denote)           
      ("f" "find-file" project-denote-find-file)      
      ("l" "link" project-denote-link)      
      ("b" "backlinks" project-denote-backlinks) 
      ("d" "dired" project-denote-dired)     
      ("g" "grep" project-denote-grep))     
    "List of choice used by project-denote-menu, which uses char-choice-menu"
    )
  
  (defun project-denote-menu ()
    "Menu made with char-choice menu to be used in project-switch-commands and
to bind to project-prefix-map."
    (interactive)
    (char-choice-menu project-denote-char-choice-list "Denote menu: "))
  
  (add-to-list 'project-switch-commands '(project-denote-menu "Denote menu"))

  (defun global-denote-dir ()
    (setq denote-directory (get-all-note-directories)))

  (defun global-denote-find-file ()
    "Call denote-find-file after setting denote-directory to a list of all note
directories (default and projects)." 
    (interactive)
    (global-denote-dir)
    (call-interactively 'denote-find-file))
  
  (defun global-denote-link ()
    "Call denote-link after setting denote-directory to a list of all note
directories (default and projects)." 
    (interactive)
    (global-denote-dir)
    (call-interactively 'denote-link))

  (defun global-denote-backlinks ()
    "Call denote-backlinks after setting denote-directory to a list of all note
directories (default and projects)." 
    (interactive)
    (global-denote-dir)
    (call-interactively 'denote-backlinks))

  (defun global-denote-dired ()
    "Call denote-dired after setting denote-directory to a list of all note
directories (default and projects)." 
    (interactive)
    (global-denote-dir)
    (call-interactively 'denote-dired))

  (defun global-denote-grep ()
    "Call denote-grep after setting denote-directory to a list of all note
directories (default and projects)." 
    (interactive)
    (global-denote-dir)
    (call-interactively 'denote-grep))
  
  ;; Automatically rename Denote buffers when opening them so that
  ;; instead of their long file name they have, for example, a literal
  ;; "[D]" followed by the file's title.  Read the docstring of
  ;; `denote-rename-buffer-format' for how to modify this.
  (denote-rename-buffer-mode 1))
