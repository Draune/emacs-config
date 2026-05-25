(use-package org
  :defer t 
  :config
  (setq org-hide-emphasis-markers t
	org-pretty-entities t
	org-agenda-files '("~/org")
	org-agenda-window-setup 'current-window
	org-log-done t
	)
  
  ;; Resize Org headings
  (dolist (face '((org-level-1 . 1.1)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.1)
                  (org-level-4 . 1.1)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :weight 'bold :height (cdr face)))

  ;; Make the document title a bit bigger
  (set-face-attribute 'org-document-title nil :weight
		      'bold :height 1.8)

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
  :config
  (setopt org-modern-star 'replace)
  (setopt org-modern-replace-stars "◉○◈◇✳")
  (global-org-modern-mode)
  )
