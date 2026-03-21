;;; lemon-monitor.el --- Monitor classes             -*- lexical-binding: t; -*-

;; Copyright (C) 2019, 2020, 2021 Ian Eure
;; Copyright (C) 2015 zk_phi

;; Author: zk_phi
;; Author: Ian Eure <ian@retrospec.tv>
;; Keywords: extensions, unix

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This file contains the base code for Lemon monitors.

;;; Code:

(require 'ring)
(require 'eieio)
(require 'lemon-sparkline)

 ;; I/O helpers

(defun lemon-monitor--make-history-ring (size)
  "like `(make-ring size)' but filled with `nil'."
  (cons 0 (cons size (make-vector size nil))))

(defun lemon-monitor--linux-read-lines (file reader indices)
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char 1)
    (mapcar (lambda (index)
              (save-excursion
                (when (search-forward-regexp (concat "^" index "\\(.*\\)$") nil t)
                  (if reader
                      (funcall reader (match-string 1))
                    (match-string 1)))))
            indices)))

(defun lemon-monitor--slurp (file)
  "Return the contents of FILE as a string."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-substring (point-min) (line-end-position))))


 ;; Process management

(defvar lemon--process-buffer-name " *lemon-process*")
(defvar lemon--process-reference-count 0)

(defun lemon--read-value-from-process-buffer (index)
  "Read a value from a specific buffer"
  (when (get-buffer lemon--process-buffer-name)
    (with-current-buffer lemon--process-buffer-name
      (when (save-excursion
              (search-backward-regexp (concat index ":\\([0-9]+\\)\\>") nil t))
        (read (match-string 1))))))

(defun lemon--maybe-start-process (cmd)
  (setq lemon--process-reference-count
        (1+ lemon--process-reference-count))
  (unless (get-buffer lemon--process-buffer-name)
    (let ((proc (start-process-shell-command
                 "lemon-process" lemon--process-buffer-name cmd))
          (filter (lambda (_ str)
                    (when (get-buffer lemon--process-buffer-name)
                      (with-current-buffer lemon--process-buffer-name
                        (when (and (string-match "-" str) (search-backward "----" nil t))
                          (delete-region 1 (point)))
                        (goto-char (1+ (buffer-size)))
                        (insert str))))))
      (set-process-query-on-exit-flag proc nil)
      (set-process-filter proc filter))))

(defun lemon--maybe-kill-process ()
  (setq lemon--process-reference-count
        (1- lemon--process-reference-count))
  (when (and (zerop lemon--process-reference-count)
             (get-buffer lemon--process-buffer-name))
    (kill-buffer lemon--process-buffer-name)))


 ;; Class definitions

;;;###autoload
(defclass lemon-monitor ()
  ((interval :type integer
             :initform 4
             :initarg :interval
             :documentation "Fetch interval in seconds.")
   (display-opts :type list
                 :initform nil
                 :initarg :display-opts
                 :documentation "User-specified display options for this monitor.")
   (default-display-opts :type list
     :documentation "Default display options for this monitor.")

   ;; Internal slots

   (timer
    :documentation "Fires `lemon-monitor-fetch' for this monitor.")
   (value
    :accessor lemon-monitor-value
    :documentation "Most recent fetched value.")
   (fetch-errors-warned
    :initform nil
    :documentation "List of errors from fetching.

This is a list of symbols of errors signaled when calling
`lemon-monitor-fetch' on this monitor.  When fetching a monitor's
value signals an error, it's displayed as a warning.  Subsequent
errors of the same type are suppressed.")
   (display-errors-warned
    :initform nil
    :documentation "List of errors from displaying.

This is a list of symbols of errors signaled when calling
`lemon-monitor-display' on this monitor.  When displaying a
monitor signals an error, it's displayed as a warning.
Subsequent errors of the same type are suppressed."))

  :abstract t
  :documentation "Base (default) Lemon monitor class.")

(cl-defmethod lemon-monitor--maybe-warn ((this lemon-monitor) error errors-warned type)
  (cl-destructuring-bind (error-symbol &rest _) error
    (unless (memq error-symbol (slot-value this errors-warned))
      (push error-symbol (slot-value this errors-warned))
      (warn "%s of %s failed: %s"
            type (eieio-object-class this) error-symbol)
      ;; Evaluate to nil -- fetch failed, so there's no value.
      nil)))

(cl-defmethod lemon-monitor-update ((this lemon-monitor))
  "Update THIS, storing the latest value."
  (oset this value
        (condition-case error
            (lemon-monitor-fetch this)
          (error (lemon-monitor--maybe-warn this error 'fetch-errors-warned "Update")))))

(defun lemon-monitor--plist-merge (defaults user)
  (let ((opts (cl-copy-list defaults))
        (user (cl-copy-list user)))
    (while user
      (let ((k (pop user))
            (v (pop user)))
        (setq opts
              (plist-put opts k
                         (if (consp v)
                             ;; If user options are a list, recursively merge.
                             (lemon-monitor--plist-merge (plist-get opts k) v)
                           v)))))
    opts))

(cl-defmethod initialize-instance :after ((this lemon-monitor) &rest _)
  ;; Merge display opts
  (when (slot-boundp this 'default-display-opts)
    (with-slots (display-opts default-display-opts) this
      (setf display-opts (lemon-monitor--plist-merge
                          default-display-opts
                          display-opts)))))

(cl-defmethod lemon-monitor-setup ((this lemon-monitor))
  "Setup this monitor.

This method is called when activating `lemon-mode'."

  (oset this timer
        (run-with-timer 0 (oref this interval)
                        (apply-partially #'lemon-monitor-update this))))

(cl-defmethod lemon-monitor-cleanup ((this lemon-monitor))
  "Cleanup the monitor.

   This method is called when deactivating `lemon-mode'."
  (when (slot-boundp this 'timer)
    (cancel-timer (oref this timer))
    (oset this timer nil)))

(cl-defmethod lemon-monitor-fetch ((this lemon-monitor))
  "Fetch the current monitor value."
  (error "No `lemon-monitor-fetch' defined for monitor %s" (eieio-object-class this)))

(cl-defmethod lemon-monitor-display ((this lemon-monitor))
  "Default display method for Lemon monitors."
  (with-slots (display-opts) this
    (let ((val (lemon-monitor-value this))
          (index (or (plist-get display-opts :index) ""))
          (unit (or (plist-get display-opts :unit) "")))
      (concat index
              (if (not (numberp val)) "N/A"
                (format "%d%s" val unit))))))


 ;; History monitor

(defclass lemon-monitor-history (lemon-monitor)
  ((history-size :type integer :custom integer
                 :initform 50
                 :initarg :history-size)

   (default-display-opts :initform nil)

   ;; Internal slots

   (history
    :accessor lemon-monitor-history-history
    :documentation "Ring of historical monitor values")
   (sparkline
    :initform nil
    :documentation "Sparkline rendering instance for this monitor."))

  :abstract t
  :documentation "Monitor class which stores a history of values.")

(cl-defmethod initialize-instance :before ((this lemon-monitor-history) &rest)
  "Initialize default display options."
  (plist-put (oref this default-display-opts) :sparkline '(:type gridded)))

(cl-defmethod initialize-instance :after ((this lemon-monitor-history) &rest _)
  (with-slots (history-size display-opts history sparkline) this
    (setf history (lemon-monitor--make-history-ring history-size))
    (when-let ((sparkline-opts (plist-get display-opts :sparkline)))
      (setf sparkline (apply #'lemon-sparkline-new sparkline-opts)))))

(cl-defmethod lemon-monitor-history-history ((this lemon-monitor-history))
  (oref this history))

(cl-defmethod lemon-monitor-value ((this lemon-monitor-history))
  (car (ring-elements (lemon-monitor-history-history this))))

(cl-defmethod lemon-monitor-update :around ((this lemon-monitor-history))
  (ring-insert (oref this history) (lemon-monitor-fetch this)))

(cl-defmethod lemon-monitor-display ((this lemon-monitor-history))
  "Default display method for Lemon monitors."
  (with-slots (sparkline) this
      (concat
       (cl-call-next-method)
       (when (and sparkline (window-system))
         (concat " "
                 (propertize " " 'display (lemon-sparkline-graph sparkline (ring-elements (lemon-monitor-history-history this)))))))))

(provide 'lemon-monitor)
;;; lemon-monitor.el ends here
