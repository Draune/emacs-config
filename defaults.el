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

;; Setup vertical display of completion (juste native stuff)
;; 1) because vetico + fussy freeze Emacs when using tramp
;; 2) because ido-vertical-mode don't do it for commands
;; 3) consult seems to much comparing to what I need (and seems like complicated to configure)
(fido-mode)
(icomplete-vertical-mode)
;; Disble the displaying of *Completions* since I use icomplete
(setq completion-auto-help nil)
;; Here because by default TAB calls the default completion help (which I deactivated but I don't want it to be useless)
(bind-key "<tab>" 'icomplete-fido-ret icomplete-fido-mode-map)

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

;; Better completion (lines found in Vertico github pages)
(setq completion-styles '(basic substring partial-completion flex))
(setq completion-ignore-case t)

;; Install and use ef-themes
(use-package ef-themes
  :ensure t
  :defer t
  )
(add-hook 'after-init-hook (lambda () (load-theme 'ef-melissa-light t)))

;; Install rainbow-limeters 
(use-package rainbow-delimiters
  :ensure t
  :defer t
  )
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
