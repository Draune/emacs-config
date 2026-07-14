;; All independant tools (that need to be called to do something)

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

(use-package ace-window
  :defer t
  :commands
  ace-window
  :bind
  ("M-o" . ace-window)
  :config
  (setq aw-keys '(?s ?d ?f ?g ?h ?j ?k ?l))
  ;; to get it to be visible with EXWM buffers
  (advice-add 'ace-window :before (lambda (&rest args)
				    (ace-window-display-mode 1)))
  (advice-add 'ace-window :after (lambda (&rest args)
				    (ace-window-display-mode -1)))
  )
