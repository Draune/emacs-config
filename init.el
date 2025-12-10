;; What is installed:
;; - use-package
;; - vertico
;; - orderless
;; - savehist
;; - doom-themes
;; - corfu
;; - magit
;; - eat
;; - devil
;; - f
;; - exwm (just if Emacs was called by xinit/custom script/display manager)
;; - lemon
;; - rainbow-delimiters

;; What other thing there is:
;; - Use of Melpa archives
;; - Disable the splash screen
;; - Disable bad GUI elements
;; - Enable global-visual-line-mode
;; - Customize startup *scratch* buffer
;; - Configuration of which-key
;; - Highlight current line
;; - Disable cursor blinking
;; - Increase font size
;; - Delete marked region when editing
;; - Move elsewhere backups and auto-saves
;; - My keybindings

;; Add Melpa archives (from the geting started page of the MELPA website)
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; Setup use-package from reddit https://www.reddit.com/r/emacs/g2m2rg/a_guide_to_configure_emacs (anser from magicalmurray)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))

;; Load configs

;; Utilities functions
(load-file "~/.emacs.d/util.el")
;; Tools
(load-file "~/.emacs.d/tools.el")
;; Better defaults
(load-file "~/.emacs.d/defaults.el")
;; Auto-complete (Corfu)
(load-file "~/.emacs.d/auto-complete.el")
;; exwm and lemon configs (will only be load if Emacs was launched by xinit), it needs to be loaded at the end
(load-file "~/.emacs.d/wm.el")
;; Key bindings (at the end for the which-key setup)
(load-file "~/.emacs.d/key.el")

;; Automatically added things
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auth-source-save-behavior nil)
 '(package-selected-packages
   '(async corfu-terminal devil doom-themes eat ef-themes elfeed exwm f
	   google-translate lemon magit orderless rainbow-delimiters
	   vertico))
 '(package-vc-selected-packages
   '((devil :url "https://github.com/fbrosda/devil" :branch "dev"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(lemon-battery-charging-face ((t (:foreground "#ee27eb"))))
 '(lemon-battery-full-face ((t (:foreground "#ee27eb"))))
 '(lemon-battery-low-face ((t (:background "#ee27eb"))))
 '(lemon-battery-medium-face ((t (:foreground "#ffd400"))))
 '(lemon-time-face ((t (:foreground "#bd00ff")))))

