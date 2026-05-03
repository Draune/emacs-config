;; Here is all the configs directly linked to configuring emacs defaults (and the theme)
(use-package emacs
  :init
  ;; Disable bad GUI from Emacs
  (when (display-graphic-p)
    (menu-bar-mode -1)
    (scroll-bar-mode -1)
    (tool-bar-mode -1)
    )
  ;; Enable global-visual-line-mode (words aren't cut in half at the end of line)
  (global-visual-line-mode)

  (setq display-line-numbers-type 'relative)
  ;; (global-display-line-numbers-mode)

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
  (setq backup-directory-alist `(("." . "~/.emacs.d/backups"))
	backup-by-copying t
	auto-save-file-name-transforms `((".*" "~/.emacs.d/auto-saves/" t))
	create-lockfiles nil)

  (setq completion-ignore-case t)
  ;; Deactivate the bell sounds
  (setq ring-bell-function 'ignore)
  ;; To set $PATH (for ein and eshell)
  (let ((my-path (expand-file-name "~/.local/bin")))
    (setenv "PATH" (concat (getenv "PATH") ":" my-path))
    (add-to-list 'exec-path my-path))

  (if (file-exists-p "~/.emacs.d/theme.el")
      (load-relative "theme.el"))

  (setq use-short-answers t)
  (setq enable-recursive-minibuffers t)

  :bind
  ;; My keybindings
  (("C-c k" . 'kill-current-buffer)
  ;; Doublons here for better experience with Azerty keyboards 
  ("C-x \"" . 'split-window-right)
  ("C-x é" . 'split-window-below)
  ("C-x à" . 'delete-window)
  ("C-x &" . 'delete-other-windows)

  ;; better functions fo default keybindings
  ("C-k" . 'kill-whole-line)
  ("M-n" . (lambda () (interactive) (next-line 10)))
  ("M-p" . (lambda () (interactive) (previous-line 10)))
  ("C-o" . (lambda () (interactive)
		    (call-interactively 'move-beginning-of-line)
		    (call-interactively 'open-line)
		    (call-interactively 'indent-for-tab-command)))
  ("M-a" . 'beginning-of-buffer)
  ("M-e" . 'end-of-buffer)

  ;; Because I don't have ">" and "<" on my keyboard
  ("C-c i i" . (lambda () (interactive) (insert "<")))
  ("C-c i s" . (lambda () (interactive) (insert ">")))
  ("C-c r" . 'replace-string)
  ;; Use M-p and M-n for the command history when using M-&
  ;; Already binded but (I don't really know why) if I don't declare it
  ;; there is a bug when repeated
  :map minibuffer-local-shell-command-map
  ("M-p" . 'previous-line-or-history-element)
  ("M-n" . 'next-line-or-history-element))
  )

(use-package mood-line
  :config
  (setq mood-line-glyph-alist mood-line-glyphs-fira-code)
  (set-face-attribute 'mode-line nil :box '(:line-width (1 . 5) :style flat-button))
  (set-face-attribute 'mode-line-active nil :box '(:line-width (1 . 5) :style flat-button))
  (set-face-attribute 'mode-line-inactive nil :box '(:line-width (1 . 5) :style flat-button))
  (mood-line-mode))

(use-package topspace
  :defer t
  :autoload
  global-topspace-mode
  :config
  (defun my/topspace-active-p ()
    (not (memq major-mode '(vterm-mode org-mode))))
  :custom
  (topspace-active #'my/topspace-active-p)
  :hook
  ('after-init-hook . #'global-topspace-mode)
  )

(use-package centered-cursor-mode
  :defer t
  :autoload
  global-centered-cursor-mode
  :hook
  ('after-init-hook . #'global-centered-cursor-mode)
  :config
  (setq ccm-recenter-at-end-of-file t)
  :hook
  ('vterm-mode-hook .
		    (lambda ()                       
		      (centered-cursor-mode -1)
		      ))
  )

