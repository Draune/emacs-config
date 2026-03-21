;;; lemon-emms.el --- Lemon monitor for EMMS         -*- lexical-binding: t; -*-

;; Copyright (C) 2020, 2021  Ian Eure

;; Author: Ian Eure <ian@retrospec.tv>
;; Keywords: multimedia

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
(require 's)
(require 'url-parse)
(require 'emms)
(require 'emms-playing-time)

(defgroup lemon-emms nil
  "EMMS monitor for Lemon."
  :group 'lemon)

(defface lemon-emms-artist-face
  '((t :inherit emms-browser-artist-face))
  "Face for artist names."
  :group 'lemon-emms)


(defface lemon-emms-title-face
  '((t :inherit emms-playlist-track-face))
  "Face for track titles."
  :group 'lemon-emms)

(defface lemon-emms-time-face
  '((t :inherit default))
  "Face for playing time."
  :group 'lemon-emms)

;;;###autoload
(defclass lemon-emms (lemon-monitor) nil)

(cl-defmethod lemon-monitor-fetch ((_ lemon-emms))
  (when (and (featurep 'emms) emms-player-playing-p)
    (emms-playlist-current-selected-track)))

(defun lemon-emms--merge (track)
  (unless (memq 'info-artist track)
    (push '(info-artist) track))
  (unless (memq 'info-title track)
    (push '(info-title) track))
  track)

(defun lemon-emms--display-file (track)
  (let ((info-artist (cdr (assoc 'info-artist track)))
        (info-title (cdr (assoc 'info-title track)))
        (name (cdr (assoc 'name track))))
      (s-join " " (list
               (propertize (s-trim emms-playing-time-string) 'face 'lemon-emms-time-face)
               (s-join " - "
                       (or (cl-remove-if 'null (list (when info-artist (propertize info-artist 'face 'lemon-emms-artist-face))
                                                     (when info-title (propertize info-title 'face 'lemon-emms-title-face))))
                           (list (propertize (file-name-base name) 'face 'lemon-emms-title-face))))))))


(defun lemon-emms--display-url (track)
  (let ((pu (url-generic-parse-url (cdr (assoc 'name track)))))
    (propertize (url-host pu) 'face 'lemon-emms-title-face)))

(cl-defmethod lemon-monitor-display ((this lemon-emms))
  (if-let ((track (lemon-monitor-value this)))
    (concat
     (cond
      (emms-player-playing-p "▶ ")
      (emms-player-paused-p "⏸ ")
      (t ""))

     (pcase (cdr (assoc 'type track))
      ('file (lemon-emms--display-file track))
      ('url (lemon-emms--display-url track))))))


(provide 'lemon-emms)
;;; lemon-emms.el ends here
