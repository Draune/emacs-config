;; All independant tools (that need to be called to do something)

(use-package speed-type
  :ensure t
  :defer t
  :bind
  ("C-c s f" . 'speed-type-file-continue)
  ("C-c s p" . 'speed-type-continue-at-point)
  :commands
  (my/speed-type-continue
   speed-type-text
   speed-type-continue)
  :config
  (setq speed-type-default-lang "fr"
	speed-type-randomize nil)
  :init
  (defcustom speed-type-file-continue--default-entry
    "~/.emacs.d/speed-type/ .txt "
    "Default entry when searching for a speed-type file, I use this one because
I use orderless.")
  (defun speed-type-file-continue (file)
    "Interactive function that will prompt the user for a file to use with
speed-type (use `speed-type-file-continue--default-entry' to change the
default value entered)"
    (interactive
     (list (read-file-name
	    "Find speed-type file: "
	    speed-type-file-continue--default-entry)))
    (speed-type-continue nil file))
  (defun speed-type-continue-at-point--point (filename &rest r)
    "Function used in `speed-type-continue-at-point' to replace the
`speed-type--find-last-continue-at-point-in-stats' function."
    (with-current-buffer (get-file-buffer filename)
      (point)))
  (defun speed-type-continue-at-point ()
    "Will try to run `speed-type-continue' in the current buffer but will start at
current point instead of the last saved position for this file."
    (interactive)
    (let ((file (buffer-file-name)))
      (when (file-exists-p file)
	(advice-add 'speed-type--find-last-continue-at-point-in-stats
		    :override
		    'speed-type-continue-at-point--point)
	(speed-type-file-continue file)
	(advice-remove 'speed-type--find-last-continue-at-point-in-stats
		       'speed-type-continue-at-point--point))))
  )

(use-package languagetool
  :ensure t
  :defer t
  :commands (languagetool-check
             languagetool-clear-suggestions
             languagetool-correct-at-point
             languagetool-correct-buffer
             languagetool-set-language
             languagetool-server-mode
             languagetool-server-start
             languagetool-server-stop)
  :config
  (setq languagetool-java-arguments '("-Dfile.encoding=UTF-8")
        languagetool-console-command "~/.languagetool/languagetool-commandline.jar"
        languagetool-server-command "~/.languagetool/languagetool-server.jar")
  :bind
  ("C-c c c" . 'languagetool-check)
  ("C-c c p" . 'languagetool-correct-at-point)
  )

(use-package vterm  :defer t
  :ensure t
  :defer t
  :bind
  (("C-c t" . vterm)
   :map vterm-mode-map
   ("C-b" . vterm-send-left)
   ("C-f" . vterm-send-right)
   ("C-p" . vterm-send-up)
   ("C-n" . vterm-send-down)
   ("C-r" . vterm-send-C-r)
   ("C-d" . vterm-send-C-d)
   ("C-a" . vterm-send-C-a)
   ("C-e" . vterm-send-C-e)
   ("C-c i s" . (lambda () (interactive) (vterm-insert ">")))
   ("C-c i i" . (lambda () (interactive) (vterm-insert "<")))
   ("C-SPC" . (lambda () (interactive) (vterm-insert ", ")))
   ("C-c C-c" . (lambda () (interactive) (vterm-send "C-c")))
   ("C-k" . (lambda () (interactive) (vterm-send "C-a") (vterm-send "C-k")))
   :map project-prefix-map
   ("t" . project-vterm))
  :commands
  vterm
  project-vterm
  :config
  (require 'project)
  (defun project-vterm ()
  "Start Vterm in the current project's root directory.
If a buffer already exists for running Vterm in the project's root,
switch to it.  Otherwise, create a new Vterm buffer.
With \\[universal-argument] prefix arg, create a new Vterm buffer even
if one already exists."
  (interactive)
  (defvar vterm-buffer-name)
  (let* ((default-directory (project-root (project-current t)))
         (vterm-buffer-name (project-prefixed-buffer-name "vterm"))
         (vterm-buffer (get-buffer vterm-buffer-name)))
    (if (and vterm-buffer (not current-prefix-arg))
        (pop-to-buffer vterm-buffer (bound-and-true-p display-comint-buffer-action))
      (vterm current-prefix-arg))))
  (add-to-list 'project-switch-commands '(project-vterm "Vterm"))
  )

(use-package pdftotext
  :ensure t
  :vc (:url "https://github.com/tecosaur/pdftotext.el")
  :defer t
  :commands
  pdftotext-mode
  )
