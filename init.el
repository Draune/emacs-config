;; so Emacs will not try to write things in init.el (it's anoying when using nix
;; because of the read-only files) 
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(setq package-install-upgrade-built-in t)

(require 'use-package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(use-package compat :ensure t)

;; load configs
;; Utilities functions
(load "~/.emacs.d/util.el")
;; Tools
(load "~/.emacs.d/tools.el")
;; Better defaults
(load "~/.emacs.d/defaults.el")
;; exwm and lemon configs (will only be load if Emacs was launched by xinit), it
;; needs to be loaded at the end 
(load "~/.emacs.d/wm.el")
;; Key bindings (at the end for the which-key setup)
(load "~/.emacs.d/key.el")
;; LSP, corfu, etc.
(load "~/.emacs.d/prog.el")
(load "~/.emacs.d/org.el")
(load "~/.emacs.d/notifications.el")
(load "~/.emacs.d/reverse.el")
(load "~/.emacs.d/completion.el")
