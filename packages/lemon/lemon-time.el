;;; lemon-time.el --- Clock for Lemon                -*- lexical-binding: t; -*-

;; Copyright (C) 2019, 2021  Ian Eure

;; Author: Ian Eure <ian@retrospec.tv>
;; Keywords: calendar

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

;; Display a clock in Lemon.

;;; Code:

(require 's)
(require 'lemon-monitor)

(defgroup lemon-time nil
  "Display the current time in Lemon."
  :group 'lemon)

(defface lemon-time-face
  '((t :foreground "blue"))
  "Face for Lemon time display."
  :group 'lemon-time)

(defun lemon-time--symbol (hh mm)
  "Return Unicode clock face glyph for time HH MM."

  ;; Unicode clock face glyphs are arranged in two blocks.  Whole
  ;; hours, from 1:00 - 12:00 from #x1f550 - #x1f55b; then half-hours
  ;; 1:30 - 12:30 from #x1f55c - #x1f567.
  ;;
  ;; The approach is to start at 12:30 and subtract the correct number
  ;; of positions to represent the current time.
  ;;
  ;; Hours are simple: Normalize it to 12-hour time by taking the
  ;; modulo of the hour and 12, then subtract that from 12 to get the
  ;; offset from the end of the list of glyphs.
  ;;

  (let ((h-offset (- 12 (% hh 12)))
        (m-offset 0))                   ; Default to hh:30

    (when (>= mm 45)                    ; Round up to the next hour
      (setq h-offset (- 12 (% (1+ hh) 12))
            m-offset 12))               ; hh:00
    (when (<= mm 15)                    ; Round the current hour down
      (setq m-offset 12))               ; hh:00

    (string (- #x1f567 h-offset m-offset))))

(defun lemon-time--format (time format)
  "Format TIME according to FORMAT.

FORMAT is a superset of `format-time-string':

%f is a Unicode clock face representing the approximate current
time."
  (thread-first
      (s-replace "%f"
                 (pcase (decode-time time)
                   (`(,_ ,mm ,hh . ,_) (lemon-time--symbol hh mm)))
                 format)
    (format-time-string time)))

;;;###autoload
(defclass lemon-time (lemon-monitor)
  ((default-display-opts
     :initform '(:format "%H:%M")
     :documentation "Default options for time display.

:format is passed to `lemon-time--format' to determine what
information to display."))

  :documentation "Display the time in Lemon.")

(cl-defmethod lemon-monitor-fetch ((this lemon-time))
  "Fetch the current time."
  (current-time))

(cl-defmethod lemon-monitor-display ((this lemon-time))
  "Display the current time according to the configured format."
  (thread-first
      (lemon-time--format (lemon-monitor-value this) (plist-get (oref this display-opts) :format))
    (propertize 'face 'lemon-time-face)))

(provide 'lemon-time)
;;; lemon-time.el ends here
