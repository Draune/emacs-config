;; TODO list:
;; - async all dired task:
;;     - Copy
;;     - Move
;;     - Delete
;;     - Compress
;; - complete all exwm settings
;; - use gpg for passwd management
;; - install gnus mail client

;; What is installed:
;; - use-package
;; - ef-themes
;; - corfu
;; - corfu-terminal
;; - magit
;; - eat
;; - devil
;; - f
;; - elfeed
;; - exwm (just if Emacs was called by xinit)
;; - lemon
;; - async

;; What other thing there is:
;; - Use of Melpa archives
;; - Disable the splash screen
;; - Disable bad GUI elements
;; - Enable global-visual-line-mode
;; - Customize startup *scratch* buffer
;; - Use fido-mode and icomplete-vertical-mode (native functions) to get an equivalence of vertico
;; - Configuration of which-key
;; - Highlight current line
;; - Disable cursor blinking
;; - Increase font size
;; - Delete marked region when editing
;; - Move elsewhere backups and auto-saves
;; - Better completion
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

;; Enable global-visual-line-mode (words aren't cut in half at the end of line)
(global-visual-line-mode)

;; Customize startup showing version of Emacs and init time
(defun emacs-ppid ()
  "Retourne le PID (string) du processus parent d’Emacs."  
  (string-trim
   (shell-command-to-string
    (format "ps -o ppid= -p %d" (emacs-pid)))))

(defun emacs-parent-name ()
  "Retourne le nom du processus parent d’Emacs (Linux uniquement)."
  (string-trim
   (shell-command-to-string
    (format "ps -o comm= -p %s" (emacs-ppid)))))

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

;; My keybindings
;; Global keybindings:
(bind-key "C-c e" 'eshell)
(bind-key "C-c f" 'elfeed)
(bind-key "C-c w" 'eww)
(bind-key "C-c t" 'google-translate-buffer)

;; + devil repeatable keys
(bind-key "C-c n" 'next-buffer)
(bind-key "C-c p" 'previous-buffer)
(bind-key "C-c k" 'kill-current-buffer)
;; Doublons here for better experience with Azerty keyboards 
(bind-key "C-x \"" 'split-window-right)
(bind-key "C-x é" 'split-window-below)
(bind-key "C-x à" 'delete-window)
(bind-key "C-x &" 'delete-other-windows)

;; better functions fo default keybindings
(bind-key "C-k" 'kill-whole-line)
(bind-key "C-v" (lambda () (interactive) (next-line 10)))
(bind-key "M-v" (lambda () (interactive) (previous-line 10)))
(bind-key "C-o" (lambda () (interactive)
       (call-interactively 'move-beginning-of-line)
       (call-interactively 'open-line)
       (call-interactively 'indent-for-tab-command)))
(bind-key "M-a" 'beginning-of-buffer)
(bind-key "M-e" 'end-of-buffer)

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
  (devil-exit-key "q")
  (devil-all-keys-repeatable t)
  (devil-highlight-repeatable t)
  (devil-repeatable-keys '(("%k p" "%k n" "%k b" "%k f" "%k a" "%k e")
			   ;; repeatable keys for window and buffer management and find-file and dired
			   ;; I went a little crazy here but since most of the time actions on buffers
			   ;; and windows are followed by other commands like "C-n" I believe it's ok
			   ("%k c n" "%k c p""%k c k" "%k x o" "%k x &" "%k x é" "%k x \"" "%k x à" "%k x %k f" "%k x d" "%k x b" "%k x 0" "%k x 1" "%k x 2" "%k x 3"))) 
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

;; Install f (file library, used for the banner)
(use-package f :ensure t)

;; Install elfeed (RSS reader ~ reader for web feeds like reddit, arxiv,...)
(use-package elfeed :ensure t)

(setq elfeed-feeds
      '(
        ("https://www.reddit.com/r/emacs.rss" emacs)
	("https://securite.developpez.com/rss" securite-developpez)
        ))

;; Install google-translate
(use-package google-translate :ensure t)

;; Install exwm (just if Emacs was called by xinit)
(if (equal (emacs-parent-name) "xinit")
    (progn
      (use-package exwm
	     :ensure t
	     :config
	     (setq exwm-workspace-number 4)
	     ;; Make class name the buffer name.
	     (add-hook 'exwm-update-class-hook
		       (lambda () (exwm-workspace-rename-buffer exwm-class-name)))
	     ;; Global keybindings.
	     (setq exwm-input-global-keys
		   `(([?\s-r] . exwm-reset) ;; s-r: Reset (to line-mode).
		     ([?\s-w] . exwm-workspace-switch) ;; s-w: Switch workspace.
		     ([?\s-&] . (lambda (cmd) ;; s-&: Launch application.
				  (interactive (list (read-shell-command "$ ")))
				  (start-process-shell-command cmd nil cmd)))
		     ;; s-N: Switch to certain workspace.
		     ,@(mapcar (lambda (i)
				 `(,(kbd (format "s-%d" i)) .
				   (lambda ()
				     (interactive)
				     (exwm-workspace-switch-create ,i))))
			       (number-sequence 0 9))))
	     ;; Enable EXWM
	     (exwm-wm-mode)
	     )
	   
	   ;; Install lemon (system monitor in echo area)
	   (use-package lemon
	     :ensure t
	     :vc (:url "https://codeberg.org/emacs-weirdware/lemon.git"
		       :rev :newest)
	     :config
	     (setq lemon-delay 0.2)
	     (setq lemon-update-interval 2)
	     ;; to display graphics
	     (setq lemon-sparkline-use-xpm 1)
	     (setq lemon-monitors
		   '(((lemon-time :display-opts '(:format "%H:%M"))
		      (lemon-battery)
		      (lemon-cpu-linux :display-opts '(:sparkline (:type gridded)))
		      (lemon-memory-linux :display-opts '(:sparkline (:type gridded)))
		      (lemon-linux-network-rx :display-opts '(:sparkline (:type gridded)))
		      (lemon-linux-network-tx :display-opts '(:sparkline (:type gridded)))
		      )))

	     (lemon-mode 1))
	   )
  )

;; Install async (for dired-async)
(use-package async
  :ensure t
  :config (dired-async-mode 1))

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

