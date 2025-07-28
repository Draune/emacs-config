;; TODO:
;; - Function to open a file or dired over ssh (see https://andreyor.st/posts/2023-10-27-you-dont-need-a-terminal-emulator/)
;; - Move the config from .emacs to .emacs.d/init.el

;; What is installed:
;; - use-package
;; - ef-themes
;; - fussy
;; - corfu
;; - corfu-terminal
;; - magit
;; - dashboard
;; - speed-type
;; - eat
;; - devil

;; What other thing there is:
;; - Use of Melpa archives
;; - Disable bad GUI elements
;; - Use fido-mode and icomplete-vertical-mode (native functions) to get an equivalence of vertico
;; - Configuration of which-key
;; - Display line numbers

;; Add Melpa archives (from the geting started page of the MELPA website)
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; Setup use-package from reddit https://www.reddit.com/r/emacs/g2m2rg/a_guide_to_configure_emacs (anser from magicalmurray)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))

;; Disable bad GUI from Emacs
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

;; Install and use ef-themes
(use-package ef-themes
  :ensure t
  :config (load-theme 'ef-melissa-light t))

;; Install fuzzy search
(use-package fussy
  :ensure t
  ;; :defer t ; Do not defer because it will not be load otherwise 
  :config
  (fussy-setup))

;; Setup vertical display of completion (juste native stuff)
;; 1) because vetico + fussy freeze Emacs when using tramp
;; 2) because ido-vertical-mode don't do it for commands
;; 3) consult seems to much comparing to what I need (and seems like complicated to configure)
(fido-mode)
(icomplete-vertical-mode)

;; Setup which-key (key cheatsheet that is displayed during key sequences)
(which-key-mode)
(which-key-setup-minibuffer)

;; Install corfu (autocompletion)
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

  ;; Enable Corfu only for certain modes. See also `global-corfu-modes'.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

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
	corfu-auto-delay  0.1) 
  )

;; Install corfu-terminal
(use-package corfu-terminal
  :ensure t
  :config (corfu-terminal-mode +1))

;; Display line numbers
(global-display-line-numbers-mode 1)
(add-hook 'eww-mode-hook (lambda () (display-line-numbers-mode 0)))
(add-hook 'eat-mode-hook (lambda () (display-line-numbers-mode 0)))
(add-hook 'eshell-mode-hook (lambda () (display-line-numbers-mode 0)))

;; Install magit
(use-package magit
  :ensure t
  :defer t)

;; Install dashboard
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-startup-banner 4)
  (setq dashboard-center-content t)
  (setq dashboard-vertically-center-content t)
  (setq dashboard-items '
	((recents   . 5)
	 (projects  . 5)))
  )

;; Install speed-type
(use-package speed-type
  :ensure t
  :defer t)

;; Install eat (and use eshell in it)
(use-package eat
  :ensure t
  :defer t
  :init ; with this, when eshell is launched, it will be emulated in eat
  (add-hook 'eshell-load-hook #'eat-eshell-mode)
  )

;; Install devil (translate "," to "C-"; and use support of which-key from fbrosda)
(use-package devil
  :vc (:url "https://github.com/fbrosda/devil"
	    :branch "dev"
	    :rev :newest)
  :ensure t
  :demand t
  :custom
  (devil-exit-key ".")
  (devil-all-keys-repeatable t)
  (devil-highlight-repeatable t)
  (devil-repeatable-keys '(("%k p" "%k n" "%k b" "%k f" "%k a" "%k e")
                           ("%k m n" "%k m p")
                           ("%k m b" "%k m f" "%k m a" "%k m e")
                           ("%k m m f" "%k m m b" "%k m m a" "%k m m e"
                            "%k m m n" "%k m m p" "%k m m u" "%k m m d")))
  :bind
  ([remap describe-key] . devil-describe-key)
  :config
  (global-devil-mode)
  (define-key devil-mode-map (kbd ";") #'devil)
  (add-to-list 'devil-special-keys `("; ;" . ,(devil-key-executor ";")))
  (add-to-list 'devil-special-keys `("; SPC" . ,(devil-key-executor "; SPC")))
  (setq devil-translations '((", z" . "C-")
			     ("; z" . "M-")
			     (", ," . ",")
			     ("; ;" . ";")
			     ("; SPC" . "; SPC")
			     ("," . "C-")
			     (";" . "M-"))))

;; Automatically added things
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auth-source-save-behavior nil)
 '(package-selected-packages nil)
 '(package-vc-selected-packages
   '((devil :url "https://github.com/fbrosda/devil" :branch "dev"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
