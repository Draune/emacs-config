;;; lemon-fan.el --- Lemon fan monitor              -*- lexical-binding: t; -*-

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

(defgroup lemon-fan nil
  "Display fan speed in Lemon."
  :group 'lemon)

(defface lemon-fan-face
  '((t :inherit default))
  "Face for Lemon fan display."
  :group 'lemon-fan)

;;;###autoload
(defun lemon-fan-fans ()
  "Return all known fans."
  (file-expand-wildcards "/sys/class/hwmon/hwmon*/fan*_input"))

(defun lemon-fan--name (fan-device)
  "Return the name of FAN-DEVICE."
  (lemon-monitor--slurp (concat (file-name-directory fan-device) "/name")))

;;;###autoload
(defclass lemon-fan (lemon-monitor-history)
  ((fan :type string
        :initarg :fan
        :documentation "The fan to monitor")

   (default-display-opts
     :type list
     :initform '(:unit "rpm"
                       :hide t))))

(cl-defmethod lemon-monitor-fetch ((this lemon-fan))
  (with-slots (fan) this
    (read (lemon-monitor--slurp fan))))

(cl-defmethod lemon-monitor-display ((this lemon-fan))
  (let ((rpm (lemon-monitor-value this))
        (hide (plist-get (oref this display-opts) :hide)))
    (unless (and hide rpm (= rpm 0))
      (propertize (cl-call-next-method) 'face 'lemon-fan-face))))

(provide 'lemon-fan)
;;; lemon-fan.el ends here
