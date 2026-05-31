;; Install rainbow-limeters 
(use-package rainbow-delimiters
  :defer t
  :autoload
  rainbow-delimiters-mode
  :hook
  ('prog-mode-hook . #'rainbow-delimiters-mode)
  )

(use-package yasnippet
  :config
  (yas-global-mode 1))

(use-package lsp-bridge
  :config
  (global-lsp-bridge-mode)
  (defun my/lsp-bridge-format-on-save ()
    "This function add a local hook to lsp-bridge-code-format on before-save-hook"
    (add-hook 'before-save-hook 'lsp-bridge-code-format 0 t))
  :hook
  ('lsp-bridge-mode-hook . 'my/lsp-bridge-format-on-save)
  )

(use-package markdown-mode)

(use-package markdown-toc
  :config 
  (setq markdown-toc-preset 'pandoc)
  (setq markdown-command "pandoc --from markdown_github -t html5 --mathjax --highlight-style pygments --standalone")
  (setq markdown-toc-header-toc-title "**Sommaire**")
  :hook
  ;; Autogenerate and refresh TOC when saving a markdown file
  ('markdown-mode-hook . (lambda ()
			   (add-hook 'before-save-hook #'markdown-toc-generate-or-refresh-toc nil t)))
  )

(use-package git-gutter
  :hook (prog-mode . git-gutter-mode)
  :config
  (setq git-gutter:update-interval 0.02))

(use-package git-gutter-fringe
  :config
  (fringe-helper-define 'git-gutter-fr:added '(center repeated)
    "XXX....."
    "XXX....."
    "XXX....."
    "XXX.....")
  (fringe-helper-define 'git-gutter-fr:modified '(center repeated)
    "XXX....."
    "XXX....."
    "XXX....."
    "XXX.....")
  (fringe-helper-define 'git-gutter-fr:deleted 'bottom
    "X......."
    "XX......"
    "XXX....."
    "XXXX...."
    "XXXXX..."
    "XXXXXX.."
    "XXXXX..."
    "XXXX...."
    "XXX....."
    "XX......"
    "X......."))
