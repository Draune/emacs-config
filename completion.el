;; Vertico config
(use-package vertico
  :ensure t
  :defer t
  :autoload
  vertico-mode
  :hook
  ('after-init-hook . 'vertico-mode)
  :config
  (setq read-buffer-completion-ignore-case t
	read-file-name-completion-ignore-case t)
  )

;; Install orderless (config from vertico's github
(use-package orderless
  :ensure t
  :after vertico
  :config
  (setq completion-styles '(orderless basic)
	completion-category-overrides '((file (styles partial-completion)))
	completion-category-defaults nil ;; Disable defaults, use our settings
	completion-pcm-leading-wildcard t) ;; Emacs 31: partial-completion
  ;; behaves like substring 
  )

(use-package consult
  :ensure t
  :config
  (setq completion-in-region-function #'consult-completion-in-region)
  ;; line to get completion-at-point and indentation when pressing TAB or C-i
  (advice-add #'indent-for-tab-command
	      :after
	      (lambda (&rest args)
		(call-interactively 'completion-at-point)))
  )

(use-package marginalia
  :ensure t
  :config
  (marginalia-mode)
  )

