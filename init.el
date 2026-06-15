;; so Emacs will not try to write things in init.el (it's anoying when using nix because of the read-only files)
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(require 'use-package)

;; load packages
(dolist (path (file-expand-wildcards "~/.emacs.d/packages/*" t))
  (add-to-list 'load-path path))

(delete-file "~/.emacs.d/wm.elc")

(use-package compile-angel
  :config
  (setq load-prefer-newer t)
  (push "/init.el" compile-angel-excluded-files)
  (push "/lemon-battery.el" compile-angel-excluded-files)
  (push "/lsp-bridge.el" compile-angel-excluded-files)
  (compile-angel-on-load-mode 1)
  )

(use-package load-relative)
;; load configs
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
(load-relative "./org.el")
(load-relative "./reverse.el")
(load-relative "./completion.el")
