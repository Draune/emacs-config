;; Install LSP with lsp-bridge
;; Dependencies
(use-package markdown-mode
  :ensure t)
(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1)
  )

(use-package lsp-bridge
  :vc (:url "https://github.com/manateelazycat/lsp-bridge" :rev :newest)
  :ensure t
  :config
  (global-lsp-bridge-mode)
  )

(use-package nix-mode
  :ensure t
  :mode "\\.nix\\'")
