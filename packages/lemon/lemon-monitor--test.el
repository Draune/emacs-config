;;; lemon-monitor--test.el --- Tests for lemon-monitor  -*- lexical-binding: t; -*-

;; Copyright (C) 2019, 2021  Ian Eure

;; Author: Ian Eure <ian@retrospec.tv>
;; Keywords: internal

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
(require 'ert)

(ert-deftest lemon-monitor--test-fetch-error ()
  (defclass lemon-monitor--test-fetch-error (lemon-monitor) nil)
  (cl-defmethod lemon-monitor-fetch ((this lemon-monitor--test-fetch-error))
    (error "Testing"))

  (let ((m (lemon-monitor--test-fetch-error)))
    (with-slots (fetch-errors-warned) m
      (should (null (memq 'err fetch-errors-warned)))
      (should (null (lemon-monitor-update m)))
      (should (memq 'error fetch-errors-warned)))))

(ert-deftest lemon-monitor--test-display-opts ()
  (defclass lemon-monitor--test-display-opts (lemon-monitor) nil)

  (let ((m (lemon-monitor--test-display-opts)))
    (should (slot-boundp m 'display-opts))
    (should (null (oref m display-opts))))

  (let ((m (lemon-monitor--test-display-opts :display-opts '(:foo 1))))
    (should (slot-boundp m 'display-opts))
    (should (equal '(:foo 1) (oref m display-opts)))))

(ert-deftest lemon-monitor--test-default-display-opts ()
  (defclass lemon-monitor--test-default-display-opts (lemon-monitor)
    ((default-display-opts :initform '(:foo 1))))

  (let ((m (lemon-monitor--test-default-display-opts)))
    (should (slot-boundp m 'display-opts))
    (should (equal '(:foo 1) (oref m display-opts))))

  (let ((m (lemon-monitor--test-default-display-opts :display-opts '(:bar 2))))
    (should (slot-boundp m 'display-opts))
    (should (equal '(:foo 1 :bar 2) (oref m display-opts)))))

(ert-deftest lemon-monitor--test-plist-merge ()
  ;; Unique keys are added
  (should (equal '(:a 1 :b 2) (lemon-monitor--plist-merge '(:a 1) '(:b 2))))

  ;; Duplicate keys are replaced
  (should (equal '(:a 2) (lemon-monitor--plist-merge '(:a 1) '(:a 2))))

  ;; Recursive lists are merged
  (should (equal '(:a (:foo 1 :bar 2)) (lemon-monitor--plist-merge '(:a (:foo 1))
                                                                   '(:a (:bar 2))))))

(ert-deftest lemon-monitor-history--test-update ()
  (defclass lemon-monitor-history--test-update (lemon-monitor-history)
    ((n :initform 0)))

  (cl-defmethod lemon-monitor-fetch ((this lemon-monitor-history--test-update))
    (incf (slot-value this 'n)))

  (let ((m (lemon-monitor-history--test-update)))
    (should (= 1 (lemon-monitor-update m)))
    (should (= 2 (lemon-monitor-update m)))
    (should (= (lemon-monitor-update m) (lemon-monitor-value m)))
    (should (equal '(3 2 1) (seq-take (ring-elements (lemon-monitor-history m)) 3)))))

(provide 'lemon-monitor--test)
;;; lemon-monitor--test.el ends here
