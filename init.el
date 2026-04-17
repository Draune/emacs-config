;; so Emacs will not try to write things in init.el (it's anoying when using nix because of the read-only files)
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; load packages

(add-to-list 'load-path "~/.emacs.d/packages/centered-cursor-mode")
(add-to-list 'load-path "~/.emacs.d/packages/cond-let")
(add-to-list 'load-path "~/.emacs.d/packages/consult")
(add-to-list 'load-path "~/.emacs.d/packages/compile-angel")
(add-to-list 'load-path "~/.emacs.d/packages/corfu")
(add-to-list 'load-path "~/.emacs.d/packages/dash")
(add-to-list 'load-path "~/.emacs.d/packages/devil")
(add-to-list 'load-path "~/.emacs.d/packages/exwm")
(add-to-list 'load-path "~/.emacs.d/packages/f")
(add-to-list 'load-path "~/.emacs.d/packages/ht")
(add-to-list 'load-path "~/.emacs.d/packages/lemon")
(add-to-list 'load-path "~/.emacs.d/packages/llama")
(add-to-list 'load-path "~/.emacs.d/packages/load-relative")
(add-to-list 'load-path "~/.emacs.d/packages/lsp-mode")
(add-to-list 'load-path "~/.emacs.d/packages/lv")
(add-to-list 'load-path "~/.emacs.d/packages/magit")
(add-to-list 'load-path "~/.emacs.d/packages/magit-section")
(add-to-list 'load-path "~/.emacs.d/packages/markdown-mode")
(add-to-list 'load-path "~/.emacs.d/packages/markdown-toc")
(add-to-list 'load-path "~/.emacs.d/packages/orderless")
(add-to-list 'load-path "~/.emacs.d/packages/org-appear")
(add-to-list 'load-path "~/.emacs.d/packages/org-modern")
(add-to-list 'load-path "~/.emacs.d/packages/posframe")
(add-to-list 'load-path "~/.emacs.d/packages/prescient")
(add-to-list 'load-path "~/.emacs.d/packages/rainbow-delimiters")
(add-to-list 'load-path "~/.emacs.d/packages/s")
(add-to-list 'load-path "~/.emacs.d/packages/speed-type")
(add-to-list 'load-path "~/.emacs.d/packages/spinner")
(add-to-list 'load-path "~/.emacs.d/packages/topspace")
(add-to-list 'load-path "~/.emacs.d/packages/transient")
(add-to-list 'load-path "~/.emacs.d/packages/transient-posframe")
(add-to-list 'load-path "~/.emacs.d/packages/vertico")
(add-to-list 'load-path "~/.emacs.d/packages/vertico-posframe")
(add-to-list 'load-path "~/.emacs.d/packages/vertico-prescient")
(add-to-list 'load-path "~/.emacs.d/packages/vterm")
(add-to-list 'load-path "~/.emacs.d/packages/with-editor")
(add-to-list 'load-path "~/.emacs.d/packages/xelb")

(require 'use-package)

(use-package compile-angel
  :config
  (setq load-prefer-newer t)
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
