
(setq org-hide-emphasis-markers t
      org-hide-leading-stars t
      org-pretty-entities t
      org-hide-macro-markers t
      org-hide-block-startup t
      org-hide-drawer-startup t
      )
(require 'org-appear)
(add-hook 'org-mode-hook #'org-appear-mode)
(setq org-appear-autolinks t)

(dolist (face '((org-level-1 . 1.35)
                (org-level-2 . 1.3)
                (org-level-3 . 1.2)
                (org-level-4 . 1.1)
                (org-level-5 . 1.1)
                (org-level-6 . 1.1)
                (org-level-7 . 1.1)
                (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :weight 'bold :height (cdr face)))
(set-face-attribute 'org-document-title nil :weight 'bold :height 1.8)

(setq org-agenda-files '("~/org"))
(setq org-agenda-window-setup 'current-window)
(setq org-log-done t)
