(use-package org
  :defer t 
  :config
  (setq org-hide-emphasis-markers t
	org-pretty-entities t
	org-agenda-files '("~/org")
	org-agenda-window-setup 'current-window
	org-log-done t
	)
  :bind
  ("C-c a" . (lambda () (interactive) (org-agenda nil "n")))
  ("C-c l s" . 'org-store-link)
  ("C-c l i" . 'org-id-store-link)
  )

(use-package org-appear
  :after org
  :hook
  ('org-mode-hook . #'org-appear-mode)
  )

(use-package org-modern
  :after org
  :load-path "~/.emacs.d/packages/org-modern"
  :config
  (global-org-modern-mode)
  )
