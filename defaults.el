;; Here is all the configs directly linked to configuring emacs defaults (and the theme)

;; Disable bad GUI from Emacs
(when (display-graphic-p)
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  )

;; Enable global-visual-line-mode (words aren't cut in half at the end of line)
(global-visual-line-mode)

(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode)

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
       (insert (format ";; %s\n" (emacs-version)))
       (insert (format ";; Init time: \t%s\n" (emacs-init-time)))
       (if (eq system-type 'gnu/linux)
	   (insert (format ";; Started by: \t%s\n" (my/emacs-parent-name))))
       (insert (format ";; Started at: \t%s\n" (format-time-string "%T %a %d/%m/%Y")))
       (insert "\n")
       ;; (emacs-lisp-mode 1)
       )
(setq inhibit-startup-screen t)
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

;; Vertico config
(require 'vertico)
(vertico-mode)

;; Persist history over Emacs restarts. Vertico sorts by history position.
(savehist-mode)

;; Install orderless (config from vertico's github
(require 'orderless)
(setq completion-styles '(orderless basic))
(setq completion-category-overrides '((file (styles partial-completion))))
(setq completion-category-defaults nil) ;; Disable defaults, use our settings
(setq completion-pcm-leading-wildcard t) ;; Emacs 31: partial-completion behaves like substring

(require 'vertico-posframe)
(vertico-posframe-mode 1)

;; Deactivate the bell sounds
(setq ring-bell-function 'ignore)

;; Install rainbow-limeters 
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; To set $PATH (for ein and eshell)
(let ((my-path (expand-file-name "~/.local/bin")))
  (setenv "PATH" (concat (getenv "PATH") ":" my-path))
  (add-to-list 'exec-path my-path))

;; Use consult to get auto-completion in vertico for async-shell-command and launch-app for my EXWM config
(require 'consult)
(setq completion-in-region-function #'consult-completion-in-region)

;; To get an list of executables in $PATH
(defun my/update-exec-list () (interactive)
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

(if (file-exists-p "~/.emacs.d/theme.el")
    (load-relative "theme.el"))

(setq use-short-answers t)

(require 'topspace)
(global-topspace-mode 1)

(require 'centered-cursor-mode)
(global-centered-cursor-mode)
(add-hook 'vterm-mode-hook
	  (lambda ()
            (centered-cursor-mode -1)))
(add-hook 'vterm-copy-mode-hook
          (lambda ()
            (if vterm-copy-mode
                (centered-cursor-mode 1)   
              (centered-cursor-mode -1))))
