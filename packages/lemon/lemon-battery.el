;;; lemon-battery.el --- Battery monitor for lemon   -*- lexical-binding: t; -*-

;; Copyright (C) 2019, 2021  Ian Eure

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

;; Battery monitor for Lemon, using built-in battery.el.

;;; Code:

(require 'lemon)
(require 'battery)
(require 'pcase)

(defgroup lemon-battery nil
  "Customization for Lemon battery monitor."
  :group 'lemon)

(defface lemon-battery-low-face
  '((t :background "Red1"
       :foreground "white"
       :weight bold))
  "Face for when the battery is discharging, and has less than 30
minutes left before depletion."
  :group 'lemon-battery)

(defface lemon-battery-medium-face
  '((t :foreground "DarkOrange"
       :weight bold))
  "Face for when the battery is discharging, and has between 30
and 59 minutes left before depletion."
  :group 'lemon-battery)

(defface lemon-battery-full-face
  '((t :foreground "ForestGreen"))
  "Face for when the battery is discharging, and holds more than
an hour's charge."
  :group 'lemon-battery)

(defface lemon-battery-charging-face
  '((t :inherit lemon-battery-full-face))
  "Face for when the battery is being charged."
  :group 'lemon-battery)

(defun lemon-battery--capacity-face (charging percent)
  "Return monitor face based on CHARGING and PERCENT.

This is used when sysfs doesn't provide an estimate for the time
left, because there's no way to determine the discharge rate.

If the battery is being charged, returns
`lemon-battery-charging-face'

If the battery is discharging, and has >= 60% left, returns
`lemon-battery-full-face'.

If the battery is discharging, and has >= 20% left, returns
`lemon-battery-medium-face'.

If the battery is discharging, and has beween 0-19% left, returns
`lemon-battery-low-face'."
  (if charging 'lemon-battery-charging-face
    (cond
     ((>= percent 60) 'lemon-battery-full-face)
     ((>= percent 20) 'lemon-battery-medium-face)
     (t 'lemon-battery-low-face))))

(defun lemon-battery--time-face (charging time-left)
  "Return monitor face based on CHARGING and TIME-LEFT.

If the battery is being charged, returns
`lemon-battery-charging-face'

If the battery is discharging, and there's between 59-30 minutes
estimated time to depletion, returns `lemon-battery-medium-face'.

If the battery is discharging, and there's between 0-29 minutes
estimated time to depletion, returns `lemon-battery-low-face'."

  (if charging 'lemon-battery-charging-face
    (cl-destructuring-bind (hh mm) (mapcar #'read (split-string time-left ":"))
      (cond
       ((and (= hh 0) (> mm 30)) 'lemon-battery-medium-face)
       ((= hh 0) 'lemon-battery-low-face)
       (t 'lemon-battery-full-face)))))

(defun lemon-battery--face (charging percent time-left)
  (if (string= "N/A" time-left)
      (lemon-battery--capacity-face charging percent)
    (lemon-battery--time-face charging time-left)))

;;;###autoload
(defun lemon-battery-present? ()
  (file-expand-wildcards "/sys/class/power_supply/BAT*"))

;;;###autoload
(defclass lemon-battery (lemon-monitor)
  ((interval :initform 10)
   (default-display-opts
     :initform '(:charging-indicator "^"
                                     :discharging-indicator "v")))
  :documentation "Battery monitor for Lemon.")

(cl-defmethod lemon-monitor-fetch ((_ lemon-battery))
  "Return battery status."
  ;; If there are errors, ignore them -- it might be the user swapping
  ;; removeable batteries.
  (ignore-errors
    (when battery-status-function
      (funcall battery-status-function))))

(cl-defmethod lemon-battery--indicator ((this lemon-battery) charging)
  "Return the battery charging or discharging indicator."
  (with-slots (display-opts) this
    (plist-get display-opts
               (if charging :charging-indicator
                 :discharging-indicator))))

(cl-defmethod lemon-monitor-display ((this lemon-battery))
  "Return the display text for the battery monitor."
  (when-let ((status (lemon-monitor-value this)))
    (let ((charging (string= (downcase (cdr (assoc ?B status))) "charging"))
          (percent (pcase (read (cdr (assoc ?p status)))
                     ((and (pred numberp) pct) pct)
                     (_ 0)))
          (time-left (cdr (assoc ?t status))))
      (thread-first
          (format "%d%%%s"
                  percent
                  (if (string= time-left "N/A")
                      "" (concat (lemon-battery--indicator this charging) time-left)))
        (propertize 'face (lemon-battery--face charging percent time-left))))))

(provide 'lemon-battery)
;;; lemon-battery.el ends here
