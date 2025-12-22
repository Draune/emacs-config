;; All independant tools (that need to be called to do something)

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

;; Install google-translate
(use-package google-translate :ensure t :defer t)

(use-package vterm
  :ensure t
  :defer t
  )

(use-package gptel
  :ensure t
  :defer t
  :config
  (defun my/get-ollama-models ()
  "Fetch the list of installed Ollama models."
  (let* ((output (shell-command-to-string "ollama list"))
         (lines (split-string output "\n" t))
         models)
    (dolist (line (cdr lines))  ; Skip the first line
      (when (string-match "^\\([^[:space:]]+\\)" line)
        (push (match-string 1 line) models)))
    (nreverse models)))
  (defun my/update-ollama ()
    (interactive)
    (gptel-make-ollama "Ollama"             ;Any name of your choosing
    :host "localhost:11434"               ;Where it's running
    :stream t                             ;Stream responses
    :models (my/get-ollama-models))          ;List of models
    )
  (setq
   gptel-model (intern (car (get-ollama-models)))
   gptel-backend (gptel-make-ollama "Ollama"
                   :host "localhost:11434"
                   :stream t
                   :models (my/get-ollama-models))
   )
  :init
  (bind-key "C-c g m" #'gptel-menu)
  (bind-key "C-c g g" #'gptel)
  (bind-key "C-c g s" #'gptel-send)
  (bind-key "C-c g r" #'gptel-rewrite)
  (bind-key "C-c g a" #'gptel-abort)
  (bind-key "C-c g c r" #'gptel-context-remove-all)
  (bind-key "C-c g c a" #'gptel-context-add)
  (bind-key "C-c g c f" #'gptel-context-add-file)
  (bind-key "C-c g c y" #'gptel-context-add-current-kill)
  (which-key-add-key-based-replacements
  "C-c g" "gptel")
  (which-key-add-key-based-replacements
  "C-c g c" "gptel-context")
  )
