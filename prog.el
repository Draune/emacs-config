;; Install rainbow-limeters 
(use-package rainbow-delimiters
  :defer t
  :autoload
  rainbow-delimiters-mode
  :hook
  ('prog-mode-hook . #'rainbow-delimiters-mode)
  )

(use-package corfu
  :config
  (setq corfu-auto        t
	corfu-auto-delay  0 
	corfu-auto-prefix 1)
  (global-corfu-mode)
)

(use-package lsp-mode
  :defer t
  :commands
  lsp
  :hook
  ('prog-mode-hook #'lsp)
  :config
  (setq lsp-enable-snippet nil
	lsp-lens-enable nil
	lsp-modeline-diagnostics-enable nil
	lsp-modeline-code-actions-enable nil
	lsp-modeline-workspace-status-enable nil
	lsp-headerline-breadcrumb-enable nil)
  )

(use-package markdown-mode)

(use-package markdown-toc
  :config 
  (setq markdown-toc-preset 'pandoc)
  (setq markdown-command "pandoc --from markdown_github -t html5 --mathjax --highlight-style pygments --standalone")
  (setq markdown-toc-header-toc-title "**Sommaire**")
  :hook
  ;; Autogenerate and refresh TOC when saving a markdown file
  (add-hook 'markdown-mode-hook (lambda ()
				  (add-hook 'before-save-hook #'markdown-toc-generate-or-refresh-toc nil t)))
  )
