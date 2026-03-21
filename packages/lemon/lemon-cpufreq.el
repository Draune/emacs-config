;;; lemon-cpufreq.el --- CPU frequency monitor for Lemon  -*- lexical-binding: t; -*-

;; Copyright (C) 2019, 2020, 2021  Ian Eure

;; Author: Ian Eure <ian@retrospec.tv>
;; Keywords:

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
(require 'lemon-cpu)

 ;; Linux

(defun lemon-cpu--freq (cpu)
  "Return the clock frequency of CPU.

Returns a list of frequencies (MIN MAX CURRENT)."
  (cl-loop for stat in '("cpuinfo_min_freq" "cpuinfo_max_freq" "scaling_cur_freq")
           collect (read (lemon--slurp-cpu cpu "cpufreq" stat))))

(defun lemon-cpu--freqs (cpus)
  "Return the clock frequency information for CPUS."
  (mapcar #'lemon-cpu--freq cpus))

(defun lemon-cpu--minfreq (&optional cpus)
  (/ (cl-reduce #'min (mapcar #'car (lemon-cpu--freqs (or cpus (car (lemon-cpu--cpus)))))) 1000.0))

(defun lemon-cpu--maxfreq (&optional cpus)
  (/ (cl-reduce #'max (mapcar #'cadr (lemon-cpu--freqs (or cpus (car (lemon-cpu--cpus)))))) 1000.0))

;;;###autoload
(defclass lemon-cpufreq-linux (lemon-monitor-history)
  ((cpus :type list :initarg :cpus)
   (default-display-opts :initform nil)))

(cl-defmethod initialize-instance :around ((this lemon-cpufreq-linux) &rest _)
  (unless (slot-boundp this 'cpus)
    (oset this cpus (car (lemon-cpu--cpus))))

  (with-slots (cpus default-display-opts) this
    (plist-put default-display-opts :sparkline
               `(:type gridded
                       :lower-bound ,(lemon-cpu--minfreq cpus)
                       :upper-bound ,(lemon-cpu--maxfreq cpus))))
  (cl-call-next-method))

(cl-defmethod lemon-monitor-fetch ((this lemon-cpufreq-linux))
  "Return average of all cores in MHz."
  (with-slots (cpus) this
    (/ (cl-reduce #'+ (mapcar #'caddr (lemon-cpu--freqs cpus))) (length cpus) 1000.0)))

(cl-defmethod lemon-monitor-display ((this lemon-cpufreq-linux))
  (with-slots (cpus) this
    (let ((mhz (lemon-monitor-value this)))
      (if (> mhz 1000)
          (format "%.1fGHz" (/ mhz 1000.0))
        (format "%dMHz" mhz)))))

(provide 'lemon-cpufreq)
;;; lemon-cpufreq.el ends here
