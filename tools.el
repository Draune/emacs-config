;; All independant tools (that need to be called to do something)

;; Install magit
(use-package magit
  :defer t
  :bind
  ("C-x g" . 'magit)
  :commands
  magit
  )

(use-package transient-posframe
  :after transient
  :config
  (transient-posframe-mode)
  (setq transient-posframe-poshandler #'posframe-poshandler-window-bottom-center)
  )

(use-package vterm
  :defer t
  :bind
  (("C-c v" . vterm)
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
   ("v" . project-vterm))
  :commands
  vterm
  project-vterm
  :config
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
  )

(use-package speed-type
  :defer t
  :bind
  ("C-c s" . 'my/speed-type-continue)
  :commands
  (my/speed-type-continue
   speed-type-text
   speed-type-continue)
  :config
  (setq speed-type-default-lang "fr"
	speed-type-randomize nil)
  :init
  (defun my/speed-type-continue (file)
    (interactive
     (list
      (read-file-name
       "Choisir un fichier : "
       "~/.emacs.d/speed-type/ .txt ")))
    (speed-type-continue nil file)
    )
  )

(use-package multiple-cursors
  :defer t
  :commands
  mc/mark-all-in-region
  mc/edit-lines
  :bind
  ("C-c m m" . 'mc/mark-all-in-region)
  ("C-c m e" . 'mc/edit-lines))

(use-package languagetool
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

(use-package avy
  :defer t
  :commands
  avy-goto-char
  :bind
  ("C-:" . avy-goto-char))
