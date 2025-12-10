;; All independant tools (that need to be called to do something)

;; Install magit
(use-package magit
  :ensure t
  :defer t)

;; Install eat (and use eshell in it)
(use-package eat
  :ensure t
  :defer t
  :init ; with this, when eshell is launched, it will be emulated in eat
  (add-hook 'eshell-load-hook #'eat-eshell-mode)
  )

;; Install google-translate
(use-package google-translate :ensure t :defer t)

(use-package vterm
  :ensure t
  :defer t
  )
