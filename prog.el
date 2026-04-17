;; Install rainbow-limeters 
(use-package rainbow-delimiters
  :defer t
  :autoload
  rainbow-delimiters-mode
  :hook
  ('prog-mode-hook . #'rainbow-delimiters-mode)
  )

(require 'corfu)
(setq corfu-auto        t
      corfu-auto-delay  0 
      corfu-auto-prefix 1)
(global-corfu-mode)

(require 'lsp-mode)

(require 'markdown-mode)
(require 'markdown-toc)

(setq markdown-toc-preset 'pandoc)
(setq markdown-command "pandoc --from markdown_github -t html5 --mathjax --highlight-style pygments --standalone")
(setq markdown-toc-header-toc-title "**Sommaire**")

;; Autogenerate and refresh TOC when saving a markdown file
(add-hook 'markdown-mode-hook (lambda ()
				(add-hook 'before-save-hook #'markdown-toc-generate-or-refresh-toc nil t)))
