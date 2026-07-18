;; to generate offline config run :
;; export EMACS_BUILD_CONFIG=1 && emacs
(when (getenv "EMACS_BUILD_CONFIG")
  ;; if this line is not present, emacs will not upgrade compat
  (setq package-install-upgrade-built-in t)
  
  (require 'package)
  
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (delete-directory "~/.emacs.d/elpa/" t)
  (delete-directory "~/.emacs.d/config-build/" t)
  (dolist (pkg-info
	   '((compat nil) 		; to install compat-31 because of
					; problems with exwm and marginalia
	     (consult nil)
	     ("https://github.com/fbrosda/devil.git" "which-key-support")
	     (exwm nil)
	     (git-gutter-fringe nil)
	     (languagetool nil)
	     (s nil)			; lemon requires it but doesn't install
					; it when using this method
	     ("https://codeberg.org/emacs-weirdware/lemon.git" nil)
	     (marginalia nil)
	     (speed-type nil))
	   )
    (package-vc-install (pop pkg-info) (pop pkg-info))
    )

  (vc-clone (expand-file-name "~/.emacs.d/") 'Git "~/.emacs.d/config-build")
  (copy-directory "~/.emacs.d/elpa/" "~/.emacs.d/config-build/" t)
  (dolist (exclude-dir '("archives" "gnupg"))
    (delete-directory (concat "~/.emacs.d/config-build/elpa/" exclude-dir) t)
    )
  (delete-directory "~/.emacs.d/config-build/.git" t)
  (delete-file "~/.emacs.d/config-build/.gitignore")
  
  (shell-command "tar -C ~/.emacs.d/ -czf config-build.tar.gz config-build/")
  
  (kill-emacs)
  )
