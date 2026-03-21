;;; lemon.el --- tiny graphical system monitor     -*- lexical-binding: t; -*-

;; Copyright (C) 2015, 2020, 2021 zk_phi, 2019-2020 Ian Eure.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA

;; Author: ieure
;; URL: http://githhub.com/ieure/lemon
;; Version: 2.1.0

;;; Commentary:

;; Load this script
;;
;;   (require 'lemon)
;;
;; and turn on `lemon-mode'.
;;
;;   (lemon-mode)
;;
;; then a tiny system monitor is displayed in minibuffer, during idle.

;;; Change Log:

;;
;; Lemon:
;; 2.1.0 Prevent discarding messages in the echo area
;; 2.0.0 Largely rewritten, Windows and Darwin support dropped.
;; 1.0.0 first release
;; 1.1.0 add option lemon-sparkline-thickness
;; 1.1.1 add lemon-windows-page-file-monitor
;; 1.1.2 add darwin support (mac os x)
;; 1.2.0 add paging feature

;;; Code:

(require 'eieio)
(require 'cl-seq)
(require 'subr-x)
(require 'lemon-monitor)

(defconst lemon-version "2.1.0")

(defgroup lemon nil
  "Tiny graphical system monitor"
  :group 'emacs)

;; + customs

;; core

(defun lemon--set-and-restart (sym value)
  (set-default sym value)
  (when (and (boundp 'lemon-mode) lemon-mode)
    (lemon-mode -1)
    (lemon-mode 1)))

(defcustom lemon-refresh-rate 4
  "How often to obtain new values from the monitors."
  :group 'lemon
  :type 'integer
  :set 'lemon--set-and-restart)

(defcustom lemon-delay 2
  "Delay in seconds until Lemon appears."
  :group 'lemon
  :type 'integer
  :set 'lemon--set-and-restart)

(defcustom lemon-monitors
  (list '((lemon-time :display-opts '(:format "%a %b %d %H:%M"))
          (lemon-battery)
          (lemon-cpu-linux :display-opts '(:sparkline (:type gridded)))
          (lemon-memory-linux)))

  "List of list of monitors.

Each outer list is a page.  Lemon rotates through pages as it
refreshes (every LEMON-REFRESH-RATE seconds).

Each inner list is a list of monitors.  Members of that list may
be the symbol of a monitor; a direct monitor value; or an
expression which evaluates to one of those things."

  :group 'lemon
  :risky t
  :set 'lemon--set-and-restart
  :type '(repeat
          (repeat :tag "Page of monitors"
                  (choice
                   (symbol :tag "Class or object")
                   (sexp)))))

;; + utilities
;;   + general

(defun lemon--flatten (lst)
  "flatten LST"
  (cond ((null lst) lst)
        ((consp lst) (apply 'nconc (mapcar 'lemon--flatten lst)))
        (t (list lst))))

;; + lemon core

(defvar lemon--active-monitors nil)
(defvar lemon--display-active nil)
(defvar lemon--active-page    nil)
(defvar lemon--total-page-num nil)
(defvar lemon--timer-objects  nil)
(defvar lemon--faulty-monitors nil)
(defvar lemon--last-display-string nil)

(defun lemon--instantiate* (monitor-or-symbol)
  "Create an instance of a monitor, from MONITOR-OR-SYMBOL.

   MONITOR-OR-SYMBOL may be:

   - A symbol which is bound to a monitor class.  The class will be
     instantiated with no arguments.
   - A symbol which is bound to a monitor object.
   - A monitor object itself.
   - An expression which evaluates to one of the above."
  (cond
   ;; Instance of lemon-monitor class.
   ((and (eieio-object-p monitor-or-symbol) (object-of-class-p monitor-or-symbol 'lemon-monitor))
    monitor-or-symbol)

   ;; Symbol bound to a lemon-monitor class.
   ((and (symbolp monitor-or-symbol)
         (class-p monitor-or-symbol)
         (child-of-class-p monitor-or-symbol 'lemon-monitor))
    (make-instance monitor-or-symbol))

   ;; Expression which can evaluate to one of the above.
   ((consp monitor-or-symbol) (lemon--instantiate* (eval monitor-or-symbol)))
   ((null monitor-or-symbol) monitor-or-symbol)
   (t (error "Don't know how to instantiate type `%s' %s" (type-of monitor-or-symbol) monitor-or-symbol))))

(defun lemon--instantiate (pages-of-monitors)
  "Instatiate Lemon monitors in PAGES-OF-MONITORS."
  (thread-first
      (lambda (page-of-monitors)
        (cl-remove-if #'null (mapcar #'lemon--instantiate* page-of-monitors)))
    (mapcar pages-of-monitors)))

(defun lemon--initialize ()
  "Prepare Lemon to monitor the system."
  (unless lemon-monitors
    (warn "`lemon-monitors' is empty."))
  (let ((monitors (lemon--instantiate lemon-monitors)))
    (mapc #'lemon-monitor-setup (lemon--flatten monitors))
    (setq lemon--active-monitors monitors
          lemon--display-active nil
          lemon--faulty-monitors nil
          lemon--total-page-num (length lemon-monitors)
          lemon--timer-objects
          (list (run-with-timer 0 lemon-refresh-rate 'lemon--redisplay)
                (run-with-idle-timer lemon-delay t 'lemon-display)))
    (add-hook 'pre-command-hook 'lemon--display-end)
    (add-hook 'kill-emacs-hook 'lemon--cleanup)))

(defun lemon--cleanup ()
  "Clean up monitors, disabling Lemon."
  (remove-hook 'kill-emacs-hook 'lemon--cleanup)
  (remove-hook 'pre-command-hook 'lemon--display-end)
  (mapc #'cancel-timer lemon--timer-objects)
  (mapc #'lemon-monitor-cleanup (lemon--flatten lemon--active-monitors)))

(defun lemon--display-catching-errors (monitor)
  "Display MONITOR, trapping errors, if they occur."
  (condition-case e
      (lemon-monitor-display monitor)
    (error (lemon-monitor--maybe-warn monitor e 'fetch-errors-warned "Fetch"))))

(defun lemon--display-update ()
  "Display current page of monitors in the minibuffer."
  (let ((message-log-max nil)  ; do not insert to *Messages* buffer
        (display-string
         (substring
          (cl-loop for monitor in
                   (elt lemon--active-monitors lemon--active-page)
                   for output = (lemon--display-catching-errors monitor)
                   unless (or (null output) (string= "" output))
                   concat " "
                   concat output)
          1)))
    (unless (or (and (not (equal (current-message) nil))
                     (not (equal (current-message) lemon--last-display-string)))
                cursor-in-echo-area
                (active-minibuffer-window))
      (message "%s" display-string)
      (setq lemon--last-display-string display-string)
      (setq lemon--display-active t))))

(defun lemon-display ()
  "Activate lemon display."
  (interactive)
  (setq lemon--active-page 0)
  (lemon--display-update))

(defun lemon--redisplay ()
  "Update lemon display."
  (when lemon--display-active
    (setq lemon--active-page (% (1+ lemon--active-page) lemon--total-page-num))
    (lemon--display-update)))

(defun lemon--display-end ()
  "deactivate lemon display."
  (setq lemon--display-active nil))

;;;###autoload
(define-minor-mode lemon-mode
  "Tiny graphical system monitor"
  :init-value nil
  :global t
  (if lemon-mode (lemon--initialize) (lemon--cleanup)))

(provide 'lemon)

;;; lemon.el ends here
