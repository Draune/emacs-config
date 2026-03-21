;;; lemon-sparkline.el --- Sparkline generators    -*- lexical-binding: t; -*-

;; Copyright (C) 2019, 2020, 2021  Ian Eure
;; Copyright (C) 2015 zk_phi

;; Author: zk_phi
;; Author: Ian Eure <ian@retrospec.tv>
;; Keywords: multimedia, lisp

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

(require 'eieio)
(require 'cl-generic)

(defcustom lemon-sparkline-use-xpm (eq system-type 'darwin)
  "When non-nil, convert sparklines to xpm from xbm before
rendering."
  :type 'boolean
  :group 'lemon)

;;;###autoload
(defclass lemon-sparkline ()
  ((height :type integer :initarg :height :initform (truncate (* .75 (default-font-height)))
           :documentation "Graph height, in pixels.")
   (width :type integer :initarg :width :initform 80
          :documentation "Graph width, in pixels.")

   (ascent :type integer :initarg :ascent :initform 100)
   (thickness :type integer :initarg :thickness :initform 2)

   (upper-bound :type float :initarg :upper-bound :initform 100.0
                :documentation "The maximum possible value for this graph.")
   (lower-bound :type float :initarg :lower-bound :initform 0.0
                :documentation "The minimim possible value for this graph.")

   ;; Internal
   (cache :initform nil
          :documentation "Cached copy of the empty graph for this sparkline."))
  :abstract t)

(cl-defmethod lemon-sparkline-empty ((this lemon-sparkline))
  "Get base empty graph."
  (with-slots (cache) this
    (copy-sequence (or cache
        (setf cache (lemon-sparkline--make-empty this))))))

(cl-defmethod lemon-sparkline--draw-horizontal-grid ((this lemon-sparkline) vec y)
  (with-slots (width) this
    (dotimes (x/2 (/ width 2))
      (aset vec (+ (* y width) (* x/2 2)) t))))

(cl-defmethod lemon-sparkline--draw-vertical-grid ((this lemon-sparkline) vec x)
  (with-slots (height width) this
    (dotimes (y/2 (/ height 2))
      (aset vec (+ (* (* y/2 2) width) x) t))))

(cl-defmethod lemon-sparkline-graph ((this lemon-sparkline) data)
  "Graph DATA."
  (if lemon-sparkline-use-xpm
      (lemon-sparkline->xpm this data)
    (lemon-sparkline->xbm this data)))

(cl-defmethod lemon-sparkline->xbm ((this lemon-sparkline) data)
  "Graph DATA in XBM format."
  (let ((num-samples (length data)))
    (unless (zerop num-samples)
      (with-slots (height width thickness ascent upper-bound lower-bound) this
        (let* ((image-data (lemon-sparkline-empty this))
               (topmargin (1- thickness))
               (height (- height topmargin))
               (height-per-point (/ height (1+ (- upper-bound lower-bound))))
               (width-per-sample (/ width (float num-samples)))
               (samples (apply 'vector data))
               (sample nil)
               (y nil))
          (dotimes (x width)
            (setq sample (aref samples (floor (/ x width-per-sample))))
            (when (numberp sample)
              (setq y (floor (* (- sample lower-bound) height-per-point)))
              (when (and (<= 0 y) (< y height))
                (dotimes (dy thickness)
                  (aset image-data
                        (+ (* (max 0 (- height (+ y dy) 1)) width) x)
                        t)))))
          `(image :type xbm :data ,image-data :ascent ,ascent
                  :height ,height :width ,width))))))

(cl-defmethod lemon-sparkline->xpm ((this lemon-sparkline) data)
  "Convert sparkline to an xpm image."
  (with-slots (height width ascent) this
    (let ((data (plist-get (cdr (lemon-sparkline->xbm this data)) :data)))
      (with-temp-buffer
        (insert (format "/* XPM */
static char * sparkline_xpm[] = { \"%d %d 2 1\", \"@ c %s\", \". c none\""
                        width height (face-foreground 'default)))
        (let ((ix 0))
          (dotimes (_ height)
            (insert ",\n\"")
            (dotimes (_ width)
              (insert (if (aref data ix) ?@ ?.))
              (setq ix (1+ ix)))
            (insert "\"")))
        (insert "};")
        `(image :type xpm :data ,(buffer-string) :ascent ,ascent
                :height ,height :width ,width)))))

(defun lemon-sparkline--strip-type (args)
  (let ((new-args nil))
    (while args
      (let* ((k (pop args))
             (v (pop args)))
        (unless (eq k :type)
          (push k new-args)
          (push v new-args))))
    (nreverse new-args)))

;;;###autoload
(defun lemon-sparkline-new (&rest args)
  "Construct a sparkline."
  (let ((sym (intern (concat "lemon-sparkline-" (symbol-name (plist-get args :type))))))
    (unless (fboundp sym)
      (error "Unknown sparkline type `%s'" (plist-get args :type)))
    (apply sym (lemon-sparkline--strip-type args))))



(defclass lemon-sparkline-plain (lemon-sparkline) nil)

(cl-defmethod lemon-sparkline--make-empty ((this lemon-sparkline-plain))
  "Create a new empty graph for a plain sparkline."
  (with-slots (height width) this
    (make-bool-vector (* height width) nil)))



(defclass lemon-sparkline-bounded (lemon-sparkline-plain) nil)

(cl-defmethod lemon-sparkline--make-empty ((this lemon-sparkline-bounded))
  "Create a new empty graph for a boxed sparkline."
  (with-slots (height) this
    (let ((vec (cl-call-next-method)))
      (lemon-sparkline--draw-horizontal-grid this vec 0)
      (lemon-sparkline--draw-horizontal-grid this vec (1- height))
      vec)))



(defclass lemon-sparkline-boxed (lemon-sparkline-bounded) nil)

(cl-defmethod lemon-sparkline--make-empty ((this lemon-sparkline-boxed))
  "Create a new empty graph for a boxed sparkline."
  (with-slots (height width) this
    (let ((vec (cl-call-next-method)))
      (lemon-sparkline--draw-vertical-grid this vec 0)
      (lemon-sparkline--draw-vertical-grid this vec (1- width))
      vec)))



(defclass lemon-sparkline-gridded (lemon-sparkline-boxed) nil)

(cl-defmethod lemon-sparkline--make-empty ((this lemon-sparkline-gridded))
  "Create a new empty graph for a gridded sparkline."
  (with-slots (height width) this
    (let ((vec (cl-call-next-method)))
      (lemon-sparkline--draw-horizontal-grid this vec (/ height 2))
      (lemon-sparkline--draw-vertical-grid   this vec (/ width 4))
      (lemon-sparkline--draw-vertical-grid   this vec (/ width 2))
      (lemon-sparkline--draw-vertical-grid   this vec (/ (* width 3) 4))
      vec)))



(provide 'lemon-sparkline)
;;; lemon-sparkline.el ends here
