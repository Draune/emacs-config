;; Add Melpa archives (from the geting started page of the MELPA website)
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; Setup use-package from reddit https://www.reddit.com/r/emacs/g2m2rg/a_guide_to_configure_emacs (anser from magicalmurray)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))

;; Load configs

(use-package load-relative :ensure t)

;; Utilities functions
(load-relative "./util.el")
;; Tools
(load-relative "./tools.el")
;; Better defaults
(load-relative "./defaults.el")
;; exwm and lemon configs (will only be load if Emacs was launched by xinit), it needs to be loaded at the end
(load-relative "./wm.el")
;; Key bindings (at the end for the which-key setup)
(load-relative "./key.el")
;; LSP, corfu, etc.
(load-relative "./prog.el")

