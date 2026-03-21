;;; lemon-temp.el --- Temperature monitor using hwmon  -*- lexical-binding: t; -*-

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

;; Monitor system temperature(s) from hwmon.
;;
;; Core temperature example:
;;
;; (define-lemon-temperature my/coretemp (lemon-temp-coretemp-device) :index "Core:")
;; (add-to-list 'lemon-monitors 'my/coretemp)
;;
;; Wifi chipset example:
;;
;; (define-lemon-temperature my/wifitemp (lemon-temp-find-name "iwlwifi") :index "WiFi:")
;; (add-to-list 'lemon-monitors 'my/wifitemp)

;;; Code:

(require 'lemon-monitor)
(require 's)

(defgroup lemon-temp nil
  "Display sensor temperature in Lemon."
  :group 'lemon)

(defface lemon-temp-face
  '((t :inherit default))
  "Face for Lemon temperature display."
  :group 'lemon-temp)

(defun lemon-temp--sensors-enumerate (device)
  "Return a list of all sensors under DEVICE."
  (let ((default-directory device))
    (file-expand-wildcards "temp*_input")))

(defun lemon-temp--sensors-probe (device)
  "Probe sensors of DEVICE, returning only valid ones."
  (let ((default-directory device))
    (cl-loop for sensor in (lemon-temp--sensors-enumerate device)
             when (> (or (ignore-errors (read (lemon-monitor--slurp sensor))) 0) 0)
             collect (car (s-split "_" sensor)))))

(defun lemon-temp--max (device sensor)
  "Return the maximum temperature dev DEVICE sensor SENSOR can report."
  (let ((default-directory device))
    (/ (read (lemon-monitor--slurp (concat sensor "_max"))) 1000.0)))

(defun lemon-temp--min (device sensor)
  "Return the minumum temperature dev DEVICE sensor SENSOR can report."
  (let ((default-directory device))
    (or (ignore-errors (/ (read (lemon-monitor--slurp (concat sensor "_min"))) 1000.0))
        0.0)))

(defun lemon-temp--current (device sensor)
  "Return the current temperature of SENSOR under DEVICE."
  (let ((default-directory device))
    (or (ignore-errors (/ (read (lemon-monitor--slurp (concat sensor "_input"))) 1000.0))
        0.0)))

(defun lemon-temp--average (device sensors)
  "Return the average of all SENSORS under DEVICE."
  (cl-loop for sensor in sensors
           count 1 into n
           sum (lemon-temp--current device sensor) into total
           finally return (/ total n)))

(defun lemon-temp--total-max (device sensors)
  "Return the maximum possible value of all sensors SENSORS under DEVICE."
  (seq-max
   (cl-loop for sensor in sensors
            collect (lemon-temp--max device sensor))))

(defun lemon-temp--total-min (device sensors)
  "Return the minimum possible value of all sensors SENSORS under DEVICE."
  (seq-min
   (cl-loop for sensor in sensors
            collect (lemon-temp--min device sensor))))

;;;###autoload
(defun lemon-temp-coretemp-device ()
  "Find the coretemp device, if there is one."
  (or (lemon-temp-find-name "coretemp") (lemon-temp-find-name "k10temp")))

(defun lemon-temp-find-name (name)
  "Find hwmon named NAME if there is one."
  (cl-loop for device in (file-expand-wildcards "/sys/class/hwmon/hwmon*")
           for mon-name = (ignore-errors (lemon-monitor--slurp (concat device "/name")))
           when (string= name mon-name)
           return device))

(defun lemon-temp--index-name (device sensors)
  (concat (if (> (length sensors) 1)
              ;; Use the device name
              (lemon-monitor--slurp (concat device "/name"))
            ;; Use the sensor name
            (concat (lemon-monitor--slurp (concat device "/name")) " "
                    (lemon-monitor--slurp (concat device "/" (car sensors) "_label")))) ":"))

;;;###autoload
(defclass lemon-temp (lemon-monitor-history)
  ((device :type string
           :initarg :device
           :documentation "Sysfs path containing temp sensor
outputs, ex. /sys/class/hwmon/hwmon2")

   (sensors :type list
            :initarg :sensors
            :documentation
            "The specific sensors of device to monitor.  When empty,
   uses all available temperature sensors for that device.  When
   multiple sensors are specified, the average is displayed.")

   (default-display-opts :type list
     :initform '(:unit "°")))

  :documentation
  "A Lemon monitor for hwmon temperatures.")

(cl-defmethod initialize-instance :after ((this lemon-temp) &rest _)
  (with-slots (device sensors default-display-opts) this
    (unless (and (slot-boundp this 'sensors) sensors)
      (setq sensors (lemon-temp--sensors-probe device)))
    (plist-put default-display-opts
               :index (ignore-errors (lemon-temp--index-name device sensors)))))

(cl-defmethod lemon-monitor-fetch ((this lemon-temp))
  (with-slots (device sensors) this
    (lemon-temp--average device sensors)))

(cl-defmethod lemon-monitor-display ((_ lemon-temp))
  (propertize (cl-call-next-method) 'face 'lemon-temp-face))

(provide 'lemon-temp)
;;; lemon-temp.el ends here
