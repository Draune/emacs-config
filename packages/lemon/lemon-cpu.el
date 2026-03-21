;;; lemon-cpu.el --- CPU monitor for Lemon           -*- lexical-binding: t; -*-

;; Copyright (C) 2019, 2020, 2021  Ian Eure

;; Author: Ian Eure <ian@retrospec.tv>
;; Keywords: hardware

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

;;

;;; Code:

(require 'lemon-monitor)
(require 'cc-defs)

(defconst lemon-cpu--linux-path
  "/sys/devices/system/cpu/")

(defun lemon-cpu--path (cpu-num &rest stats)
  "Return the path for stat STAT on CPU CPU-NUM."
  (concat (format "%scpu%d" lemon-cpu--linux-path cpu-num)
          (when stats (concat "/" (c-concat-separated stats "/")))))

(defun lemon--slurp-cpu (cpu-num &rest stats)
  "Return value of stat STAT on CPU CPU-NUM."
  (lemon-monitor--slurp (apply #'lemon-cpu--path cpu-num stats)))

(defun lemon-cpu--range (high-maybe-low)
  "Turn range HIGH-MAYBE-LOW into a sequence of numbers."
  (if (not (string-match-p "-" high-maybe-low))
      (list (read high-maybe-low))
    (apply #'number-sequence (mapcar #'read (split-string high-maybe-low "-")))))

(defun lemon-cpu--ranges (ranges)
  "Turn RANGES into a list of discrete numbers.

   RANGES is a comma-separated string of numbers or N-M,
   representing that sequence."
  (save-match-data
    (cl-loop for range in (split-string ranges ",")
             append (lemon-cpu--range range))))

(defun lemon-cpu--cpus ()
  "Return (ONLINE-CPU-IDS . TOTAL-NUM-CPUS)."
  (cons
   (thread-first (concat lemon-cpu--linux-path "/online")
     (lemon-monitor--slurp)
     (lemon-cpu--ranges))
   (thread-first (concat lemon-cpu--linux-path "/possible")
     (lemon-monitor--slurp)
     (lemon-cpu--ranges)
     (length))))

;;;###autoload
(defclass lemon-cpu-linux (lemon-monitor-history)
  ((last-total-ticks :type integer :initform 0)
   (last-idle-ticks :type integer :initform 0)
   (default-display-opts :type list
     :initform '(:index "CPU:" :unit "%"))))

(cl-defmethod lemon-monitor-fetch ((this lemon-cpu-linux))
  (cl-destructuring-bind (cpu)
      (lemon-monitor--linux-read-lines
       "/proc/stat" (lambda (str) (mapcar 'read (split-string str nil t))) '("cpu"))
    (with-slots (last-total-ticks last-idle-ticks) this
      (let* ((total (apply '+ cpu))
             (total-diff (- total last-total-ticks))
             (idle (nth 3 cpu))
             (idle-diff (- idle last-idle-ticks)))
        (if (zerop total-diff)
            ;; If ticks haven't increased since last fetch, return the last value.
            (lemon-monitor-value this)
          (prog1 (/ (* (- total-diff idle-diff) 100) total-diff)
            (setf last-total-ticks total
                  last-idle-ticks idle)))))))

(provide 'lemon-cpu)
;;; lemon-cpu.el ends here
