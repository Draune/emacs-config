;; What is installed:
;; - use-package
;; - ef-themes
;; - corfu
;; - corfu-terminal
;; - magit
;; - eat
;; - devil
;; - treesit-auto
;; - f

;; What other thing there is:
;; - Use of Melpa archives
;; - Disable the splash screen
;; - Disable bad GUI elements
;; - Customize startup *scratch* buffer
;; - Use fido-mode and icomplete-vertical-mode (native functions) to get an equivalence of vertico
;; - Configuration of which-key
;; - Display line numbers
;; - Highlight current line
;; - Disable cursor blinking
;; - Increase font size
;; - Delete marked region when editing
;; - Better completion
;; - Setup eglot
;; - My keybindings

;; Add Melpa archives (from the geting started page of the MELPA website)
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; Here is all the configs directly linked to configuring emacs defaults

;; Disable the splash screen
(setq inhibit-startup-screen t)

;; Disable bad GUI from Emacs
(when (display-graphic-p)
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  )

;; Customize startup showing version of Emacs and init time
(defun my-scratch-init () (interactive)
       (switch-to-buffer "*scratch*")
       (end-of-buffer)
       (insert (f-read "~/.emacs.d/banner.txt"))
       (insert "\n\n")
       (insert (format ";; %s\n" (emacs-version)))
       (insert (format ";; Init time: \t%s\n" (emacs-init-time)))
       (insert (format ";; Started at: \t%s\n" (format-time-string "%T %a %d/%m/%Y")))
       (insert "\n")
       (beginning-of-buffer)
       )
(add-hook 'after-init-hook 'my-scratch-init)

;; Setup vertical display of completion (juste native stuff)
;; 1) because vetico + fussy freeze Emacs when using tramp
;; 2) because ido-vertical-mode don't do it for commands
;; 3) consult seems to much comparing to what I need (and seems like complicated to configure)
(fido-mode)
(icomplete-vertical-mode)
;; Disble the displaying of *Completions* since I use icomplete
(setq completion-auto-help nil)

;; Setup which-key (key cheatsheet that is displayed during key sequences)
(which-key-mode)
(which-key-setup-minibuffer)

;; Highlight the cursor line
(global-hl-line-mode)

;; Disable cursor blinking
(blink-cursor-mode -1)

;; Increase font size
(set-face-attribute 'default nil :height 130)

;; Delete marked region when editing (which is not activated by default)
(delete-selection-mode 1)

;; Better completion (lines found in Vertico github pages)
(setq completion-styles '(basic substring partial-completion flex))
(setq completion-ignore-case t)

;; Setup eglot
(add-hook 'c-ts-mode-hook 'eglot-ensure)
(add-hook 'c++-ts-mode-hook 'eglot-ensure)
(add-hook 'rust-ts-mode-hook 'eglot-ensure)
(add-hook 'go-ts-mode-hook 'eglot-ensure)
(add-hook 'bash-ts-mode-hook 'eglot-ensure)
(add-hook 'python-ts-mode-hook 'eglot-ensure)
(add-hook 'js-ts-mode-hook 'eglot-ensure)
(add-hook 'cmake-ts-mode-hook 'eglot-ensure)

;; My keybindings
;; Global keybindings:
(bind-key "M-RET" 'eshell)
(bind-key "M-g" 'magit)
(bind-key "M-;" 'comment-line)
(bind-key "M-a" 'beginning-of-buffer)
(bind-key "M-e" 'end-of-buffer)

;; + devil repeatable keys
(bind-key "M-n" 'next-buffer)
(bind-key "M-p" 'previous-buffer)
(bind-key "M-k" 'kill-current-buffer)
(bind-key "M-o" 'other-window)
;; Doublons here for better experience with Azerty keyboards 
(bind-key "M-\"" 'split-window-right)
(bind-key "M-3" 'split-window-right)
(bind-key "M-é" 'split-window-below)
(bind-key "M-2" 'split-window-below)
(bind-key "M-à" 'delete-window)
(bind-key "M-0" 'delete-window)

(bind-key "M-s" 'save-buffer)
(bind-key "M-f" 'find-file)
(bind-key "M-b" 'switch-to-buffer)
(bind-key "M-d" 'dired)

;; better functions fo default keybindings
(bind-key "C-k" 'kill-whole-line)
(bind-key "C-v" (lambda () (interactive) (next-line 10)))
(bind-key "M-v" (lambda () (interactive) (previous-line 10)))
(bind-key "C-o" (lambda () (interactive)
       (call-interactively 'move-beginning-of-line)
       (call-interactively 'open-line)
       (call-interactively 'indent-for-tab-command)))

;; Local keybindings:
;; Here because by default TAB calls the default completion help (which I deactivated but I don't want it to be useless)
(bind-key "<tab>" 'icomplete-fido-ret icomplete-fido-mode-map)
;; Use M-p and M-n for the command history when using M-&
;; Already binded but (I don't really know why) if I don't declare it
;; there is a bug when repeated
(bind-key "M-p" 'previous-line-or-history-element minibuffer-local-shell-command-map)
(bind-key "M-n" 'next-line-or-history-element minibuffer-local-shell-command-map)

;; Here are all the package intallations and setups

;; Setup use-package from reddit https://www.reddit.com/r/emacs/g2m2rg/a_guide_to_configure_emacs (anser from magicalmurray)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))

;; Install and use ef-themes
(use-package ef-themes
  :ensure t
  :config (load-theme 'ef-melissa-light t))

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
	corfu-auto-prefix 1
	corfu-auto-delay  0) 
  (setq corfu-bar-width 1)
  )

;; Install corfu-terminal
(use-package corfu-terminal
  :ensure t
  :config
  (corfu-terminal-mode t)
  (setq corfu-terminal-disable-on-gui nil)
  )

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

;; Install devil (translate "," to "C-"; and use support of which-key from fbrosda)
(use-package devil
  :vc (:url "https://github.com/fbrosda/devil"
	    :branch "dev"
	    :rev :newest)
  :ensure t
  :demand t
  :custom
  ;; Don't use keys that are usefull when repeated (for exemple ";" for comments)
  (devil-exit-key ".")
  (devil-all-keys-repeatable t)
  (devil-highlight-repeatable t)
  (devil-repeatable-keys '(("%k p" "%k n" "%k b" "%k f" "%k a" "%k e")
			   ;; repeatable keys for window and buffer management and find-file and dired
			   ;; I went a little crazy here but since most of the time actions on buffers
			   ;; and windows are followed by other commands like "C-n" I believe it's ok
			   ("%k %k n" "%k %k p""%k %k k" "%k %k o" "%k %k é" "%k %k \"" "%k %k à" "%k %k s" "%k %k f" "%k %k d" "%k %k b" "%k %k 0" "%k %k 2" "%k %k 3"))) 
  :bind
  ([remap describe-key] . devil-describe-key)
  :config
  (global-devil-mode)
  (global-set-key (kbd "C-,") 'global-devil-mode)
  ;; I don't really know why but if I don't do this line before calling assoc-delete-all
  ;; it will not do it. I use asoc-delete-all to delete the devil special key "%k %k" (", ,")
  ;; so ", ," will be translated to "M-". Plus this line is usefull to mark things
  (add-to-list 'devil-special-keys `(", , SPC" . ,(devil-key-executor "C-SPC")))
  (assoc-delete-all "%k %k" devil-special-keys)
  (setq devil-translations '((", , ," . "C-M-")
			     (", ," . "M-")
			     ("," . "C-")
			     )))

;; Install treesit-auto (automatically install and setup everything for
;; treesit.el, even langage grammars)
(use-package treesit-auto
  :ensure
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;; Install f (file library, used for the banner)
(use-package f :ensure t)

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

