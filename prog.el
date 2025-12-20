(use-package corfu
  :ensure t
  :config
  ;; Enable auto completion
  (setq corfu-auto        t
	corfu-auto-delay  0 
	corfu-auto-prefix 0)
  (global-corfu-mode)
  )

(use-package lsp-mode :ensure t)

(use-package nix-mode
  :ensure t
  :mode "\\.nix\\'")
