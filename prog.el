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
  :vc (:url "https://github.com/manateelazycat/lsp-bridge")
  :ensure t
  :init
  (global-lsp-bridge-mode)
  )

;; Install corfu (autocompletion just in eshell and shell commands)
(use-package corfu
  :ensure t
  :defer t
  ;; Optional customizations
  :custom
  ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  
  :hook (
	 ;; (prog-mode . corfu-mode)
         (shell-mode . corfu-mode)
         (eshell-mode . corfu-mode)
	 )

  ;; Enable Corfu only for certain modes. See also `global-corfu-modes'.
  :init

  ;; Recommended: Enable Corfu globally.  Recommended since many modes provide
  ;; Capfs and Dabbrev can be used globally (M-/).  See also the customization
  ;; variable `global-corfu-modes' to exclude certain modes.
  (global-corfu-mode)
  
  ;; Enable optional extension modes:
  (corfu-history-mode)
  ;; (corfu-popupinfo-mode)
  ;; Enable auto completion and configure quitting
  (setq corfu-auto t
	corfu-quit-no-match 'separator ;; or t
	corfu-auto-prefix 1
	corfu-auto-delay  0) 
  (setq corfu-bar-width 1)

  ;; So corfu will not be activated when vertico is active or when emacs is asking for password
  (setq global-corfu-minibuffer
      (lambda ()
        (not (or (bound-and-true-p mct--active)
                 (bound-and-true-p vertico--input)
                 (eq (current-local-map) read-passwd-map)))))
  ;; workaround to get corfu to be inactive in prog-mode while still being active everywhere else
  (add-hook 'prog-mode-hook
          (lambda ()
            (setq-local corfu-auto nil)))
  )
