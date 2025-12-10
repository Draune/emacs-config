;; Here is all the configs directly linked to configuring emacs defaults (and the theme)

;; Disable the splash screen
(setq inhibit-startup-screen t)

;; Disable bad GUI from Emacs
(when (display-graphic-p)
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  )

;; Enable global-visual-line-mode (words aren't cut in half at the end of line)
(global-visual-line-mode)

(defun my-scratch-init () (interactive)
       (switch-to-buffer "*scratch*")
       (end-of-buffer)
       (insert (f-read "~/.emacs.d/banner.txt"))
       (insert "\n\n")
       (insert (format ";; %s\n" (emacs-version)))
       (insert (format ";; Init time: \t%s\n" (emacs-init-time)))
       (if (eq system-type 'gnu/linux)
	   (insert (format ";; Started by: \t%s\n" (emacs-parent-name))))
       (insert (format ";; Started at: \t%s\n" (format-time-string "%T %a %d/%m/%Y")))
       (insert "\n")
       (beginning-of-buffer)
       )
(add-hook 'after-init-hook 'my-scratch-init)

;; Highlight the cursor line
(global-hl-line-mode)

;; Disable cursor blinking
(blink-cursor-mode -1)

;; Increase font size
(set-face-attribute 'default nil :height 130)

;; Delete marked region when editing (which is not activated by default)
(delete-selection-mode 1)

;; Move elsewhere backups and auto-saves
(if (eq (file-directory-p "~/.emacs.d/backups") nil)
    (make-directory "~/.emacs.d/backups"))
(if (eq (file-directory-p "~/.emacs.d/auto-saves") nil)
    (make-directory "~/.emacs.d/auto-saves"))
(setq backup-directory-alist `(("." . "~/.emacs.d/backups")))
(setq backup-by-copying t)
(setq auto-save-file-name-transforms `((".*" "~/.emacs.d/auto-saves/" t)))
(setq create-lockfiles nil)

(setq completion-ignore-case t)
(context-menu-mode t)

;; Install Vertico for mini-buffer vertical autocompletion
(use-package vertico
  :ensure t
  ;; :custom
  ;; (vertico-scroll-margin 0) ;; Different scroll margin
  ;; (vertico-count 20) ;; Show more candidates
  ;; (vertico-resize t) ;; Grow and shrink the Vertico minibuffer
  ;; (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
  :init
  (vertico-mode))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :ensure t
  :init
  (savehist-mode))

;; Install orderless (config from vertico's github
(use-package orderless
  :ensure t
  :custom
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch))
  ;; (orderless-component-separator #'orderless-escapable-split-on-space)
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion))))
  (completion-category-defaults nil) ;; Disable defaults, use our settings
  (completion-pcm-leading-wildcard t)) ;; Emacs 31: partial-completion behaves like substring

;; Deactivate the bell sounds
(setq ring-bell-function 'ignore)

(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-outrun-electric t)
  ;; chang hline color for something brighter because I don't see it otherwise
  (set-face-background 'hl-line "#612559")
  )

;; Install rainbow-limeters 
(use-package rainbow-delimiters
  :ensure t
  :defer t
  )
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
