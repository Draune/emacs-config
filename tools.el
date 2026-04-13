;; All independant tools (that need to be called to do something)

;; Install magit
(require 'magit)

(require 'transient-posframe)
(transient-posframe-mode)
(setq transient-posframe-poshandler #'posframe-poshandler-window-bottom-center)

(require 'vterm)

(require 'speed-type)
(setq speed-type-default-lang "fr")
(defun my/speed-type-lang-eng () (interactive)
       (setq speed-type-default-lang nil))
(defun my/speed-type-lang-fr () (interactive)
       (setq speed-type-default-lang "fr"))
(setq speed-type-randomize nil)
(defun my/speed-type-continue (file)
  (interactive
   (list
    (read-file-name
     "Choisir un fichier : "
     "~/.emacs.d/speed-type/ .txt ")))
  (speed-type-continue nil file)
  )
