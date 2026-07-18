(fido-vertical-mode 1)
(bind-key [tab] 'icomplete-fido-ret 'icomplete-fido-mode-map)

(setq completion-styles '(basic substring flex))

(use-package marginalia
  :config
  (marginalia-mode)
  )

