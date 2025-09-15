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
;; Key bindings
(load-file "~/.emacs.d/key.el")

;; Install async (for dired-async)
(use-package async
  :ensure t
  :config (dired-async-mode 1))

;; exwm and lemon configs (will only be load if Emacs was launched by xinit), it needs to be loaded at the end
(load-file "~/.emacs.d/wm.el")

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

