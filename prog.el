(use-package corfu
  :ensure t
  :config
  ;; Enable auto completion
  (setq corfu-auto        t
	corfu-auto-delay  0 
	corfu-auto-prefix 1)
  (global-corfu-mode)
  )

<<<<<<< HEAD
(use-package lsp-bridge
  :vc (:url "https://github.com/manateelazycat/lsp-bridge.git")
  :ensure t
  :config
  (global-lsp-bridge-mode)
  )
=======
(use-package lsp-mode :ensure t)
>>>>>>> parent of bda9a95 (retry lsp-bridge)

(use-package nix-mode
  :ensure t
  :mode "\\.nix\\'")
