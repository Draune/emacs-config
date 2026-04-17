;; Install devil (translate "," to "C-"; and use support of which-key from fbrosda)
(use-package devil
  :config
  (setq devil-exit-key "q"
	devil-all-keys-repeatable t
	devil-highlight-repeatable nil
	devil-repeatable-keys '(("%k p" "%k n" "%k b" "%k f" "%k a" "%k e")
			      ;; repeatable keys for window and buffer management and find-file and dired
			      ;; I went a little crazy here but since most of the time actions on buffers
			      ;; and windows are followed by other commands like "C-n" I believe it's ok
			      ("%k c k" "%k x o" "%k x &" "%k x é" "%k x \"" "%k x à" "%k x %k f" "%k x d" "%k x b" "%k x 0" "%k x 1" "%k x 2" "%k x 3")
			      ;; repeatable keys for movement M- keybindings
			      ("%k %k n" "%k %k p" "%k %k f" "%k %k b"))
	devil-translations '((", , ," . "C-M-")
			     (", ," . "M-")
			     ("," . "C-")
			     )
	devil-special-keys '(("%k h %k k" . devil-describe-key)
			     ("%k h %k l" . devil-toggle-logging))
	)
  (global-devil-mode)
  :bind
  ("C-h k" . 'devil-describe-key)
  ("C-," . (lambda () (interactive) (global-devil-mode 1)))
  ("C-SPC" . (lambda () (interactive) (devil-execute-key ", SPC")))
  ("M-SPC" . 'set-mark-command)
  )


;; Setup which-key (key cheatsheet that is displayed during key sequences)
(use-package which-key
  :defer t
  :hook
  ('after-init-hook . #'which-key-mode)
  :config
  (which-key-add-key-based-replacements "C-c l" "link")
  (which-key-add-key-based-replacements "C-c i" "insert")
  (which-key-add-key-based-replacements "C-c i i" "<")
  (which-key-add-key-based-replacements "C-c i s" ">")
  )
