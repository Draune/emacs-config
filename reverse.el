(use-package hexl
  :defer t
  :config
  (defun hexl-insert-string (str) (interactive "sString to insert: ")
	 (mapcar (lambda (char) (hexl-insert-char (string-to-char char) 1))
		 (split-string str "" t)))
  :bind
  ("C-c f x" . 'hexl-find-file)
  )
