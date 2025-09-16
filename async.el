;; The goal of this file is to get a better useer experience by using async to get non-blocking dired (copying, moving and compressing), and other things that will block Emacs

;; Install async (for dired-async)
(use-package async
  :ensure t
  ;; :config (dired-async-mode 1)
  )

(defun async-dired-do-copy () (interactive)
       (async-start
	(call-interactively 'dired-do-copy)
	(message "async-dured-do-copy ended")
	))

(bind-key "C" 'async-dired-do-copy dired-mode-map)
