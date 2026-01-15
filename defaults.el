;; Here is all the configs directly linked to configuring emacs defaults (and the theme)

;; Disable bad GUI from Emacs
(when (display-graphic-p)
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  )

;; Enable global-visual-line-mode (words aren't cut in half at the end of line)
(global-visual-line-mode)


(use-package topspace
    :ensure t
    :config
    (global-topspace-mode t)
    )

(use-package centered-cursor-mode
  :ensure t
  :config
  (setq ccm-recenter-at-end-of-file t)
  (global-centered-cursor-mode 1)
  )
;; Always keep the cursor in the midle of the screen
;; (setq scroll-preserve-screen-position t
;;       scroll-conservatively 0
;;       maximum-scroll-margin 0.5
;;       scroll-margin 99999)

(defun my/scratch-init () (interactive)
       (switch-to-buffer "*scratch*")
       (end-of-buffer)
       (if (f-exists? "~/.emacs-config/banner.txt")
	   (insert (f-read "~/.emacs-config/banner.txt"))
	 (if (f-exists? "~/.emacs.d/banner.txt")
	     (insert (f-read "~/.emacs.d/banner.txt"))
	   )
	 )
       (insert "\n")
       (insert (format "%s\n" (emacs-version)))
       (insert (format "Init time: \t%s\n" (emacs-init-time)))
       (if (eq system-type 'gnu/linux)
	   (insert (format "Started by: \t%s\n" (my/emacs-parent-name))))
       (insert (format "Started at: \t%s\n" (format-time-string "%T %a %d/%m/%Y")))
       (insert "\n")
       (beginning-of-buffer)
       (kill-buffer "*GNU Emacs*")
       (display-buffer "*scratch*")
       )
(add-hook 'after-init-hook #'my/scratch-init)

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

(use-package vertico-posframe
  :ensure t
  :config
  (vertico-posframe-mode 1)
  )

;; Deactivate the bell sounds
(setq ring-bell-function 'ignore)

(if (file-exists-p "~/.emacs.d/theme.el")
    (load-relative "./theme.el") 
  (progn
    ;; Install and use ef-themes
    (use-package ef-themes
      :ensure t
      :defer t
      )
    (add-hook 'after-init-hook (lambda () (load-theme 'ef-melissa-light t)))
    )
  )
;; (use-package doom-themes
;;   :ensure t
;;   :config
;;   (load-theme 'doom-outrun-electric t)
;;   ;; chang hline color for something brighter because I don't see it otherwise
;;   (set-face-background 'hl-line "#612559")
;;   (set-face-background 'mode-line-active "#612559")
;;   )

;; Install rainbow-limeters 
(use-package rainbow-delimiters
  :ensure t
  :defer t
  )
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; To set $PATH (for ein and eshell)
(let ((my-path (expand-file-name "~/.local/bin")))
  (setenv "PATH" (concat (getenv "PATH") ":" my-path))
  (add-to-list 'exec-path my-path))

;; Use consult to get auto-completion in vertico for async-shell-command and launch-app for my EXWM config
(use-package consult
  :ensure t
  :config
  (setq completion-in-region-function #'consult-completion-in-region)
  )

;; To get an list of executables in $PATH
(defun my/update-exec-list ()
       (setq my/exec-list nil)
       (dolist (dir exec-path)
	 (when (file-directory-p dir)
	   (let ((files (directory-files dir t)))
	     (dolist (file files)
               (when (file-executable-p file)
		 (setq-local file-name (file-name-nondirectory file))
		 (if (not (or (equal file-name ".") (equal file-name ".")))
		 (push  file-name my/exec-list)))))))
       )
(my/update-exec-list)

;; To get command completion in async-shell
(defun my/consult-async-shell-command ()
  "Run async shell command with completion on PATH executables."
  (interactive)
  (let ((command (consult--read
                  my/exec-list
                  :prompt "Async shell command: "
                  :history 'shell-command-history
                  :category 'command)))
    (async-shell-command command)))
(bind-key "M-&" 'my/consult-async-shell-command)
