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

;; Install f (file library, used for the banner)
(use-package f :ensure t)
