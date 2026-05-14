;; Vertico config
(use-package vertico
  :defer t
  :autoload
  vertico-mode
  :hook
  ('after-init-hook . 'vertico-mode)
  :config
  (setq vertico-count 6)
  )

(use-package vertico-prescient
  :after vertico
  :config
  (vertico-prescient-mode)
  (prescient-persist-mode)
  )

;; Install orderless (config from vertico's github
(use-package orderless
  :after vertico
  :config
  (setq completion-styles '(orderless basic)
	completion-category-overrides '((file (styles partial-completion)))
	completion-category-defaults nil ;; Disable defaults, use our settings
	completion-pcm-leading-wildcard t) ;; Emacs 31: partial-completion behaves like substring
  )

(use-package vertico-posframe
  :after vertico
  :if (display-graphic-p)
  ;; :hook
  ;; ('window-size-change-functions
  ;; . (lambda () () (setq vertico-posframe-width (floor (* (frame-width) (- 1.0 (* 2.0 my/vertico-posframe-margin))))))
  ;; )
  :config
  (defcustom my/vertico-posframe-margin 0.01 "Percentage of the frame size, margin on the left and right of my vertico-posframe")
  (defcustom my/vertico-posframe-vertical-pos 0.55 "Relative position in the screen")
  (defun my/vertico-posframe-poshandler (info)
    (cons (floor (* my/vertico-posframe-margin (plist-get info :parent-frame-width)))
	  (floor (* my/vertico-posframe-vertical-pos (plist-get info :parent-frame-height)))))
  (vertico-posframe-mode 1)
  (setq vertico-posframe-width (floor (* (frame-width) (- 1.0 (* 2.0 my/vertico-posframe-margin))))
	vertico-posframe-height 7
	vertico-posframe-poshandler #'my/vertico-posframe-poshandler)
  )

;; Use consult to get auto-completion in vertico for async-shell-command and launch-app for my EXWM config
(use-package consult
  :config
  (setq consult-fd-args (append consult-fd-args '("--hidden"))
	completion-in-region-function #'consult-completion-in-region)

  ;; To get an list of executables in $PATH
  (defun my/update-exec-list () (interactive)
	 (setq my/exec-list nil)
	 (dolist (dir exec-path)
	   (when (file-directory-p dir)
	     (let ((files (directory-files dir t)))
	       (dolist (file files)
		 (when (file-executable-p file)
		   (setq-local file-name (file-name-nondirectory file))
		   (if (not (or (equal file-name ".") (equal file-name ".")))
		       (push  file-name my/exec-list)))))))
	 )
  (my/update-exec-list)
  ;; To get command completion in async-shell
  (defun my/consult-async-shell-command ()
    "Run async shell command with completion on PATH executables."
    (interactive)
    (let ((command (consult--read
                    my/exec-list
                    :prompt "Async shell command: "
                    :history 'shell-command-history
                    :category 'command)))
      (async-shell-command command)))
  :bind
  ("C-c f h" . (lambda () (interactive) (consult-fd "~/"))) ;; home
  ("C-c f r" . (lambda () (interactive) (consult-fd "/"))) ;; root
  ("C-c f c" . (lambda () (interactive) (consult-fd))) ;; current
  ("M-&" . 'my/consult-async-shell-command)
  )
