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
  (("C-c v" . 'vterm)
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
   ("C-c i i" . (lambda () (interactive) (vterm-insert "<"))))
  :commands
  vterm
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
