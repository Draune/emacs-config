;; Vertico config
(use-package vertico
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
  :after vertico
  :config
  (setq completion-styles '(orderless basic)
	completion-category-overrides '((file (styles partial-completion)))
	completion-category-defaults nil ;; Disable defaults, use our settings
	completion-pcm-leading-wildcard t) ;; Emacs 31: partial-completion
  ;; behaves like substring 
  )

(use-package consult
  :after vertico
  :autoload
  consult-completion-in-region
  :config
  (setq completion-in-region-function #'consult-completion-in-region)
  )

(use-package marginalia
  :config
  (marginalia-mode)
  )

