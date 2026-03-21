;;; lemon-time--test.el --- Tests for lemon-time     -*- lexical-binding: t; -*-

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

(require 'lemon-time)
(require 'ert)

(ert-deftest lemon-time--symbol-test ()
  (string= "🕐" (lemon-time--symbol 1 0))
  (string= "🕑" (lemon-time--symbol 2 0))
  (string= "🕒" (lemon-time--symbol 3 0))
  (string= "🕓" (lemon-time--symbol 4 0))
  (string= "🕔" (lemon-time--symbol 5 0))
  (string= "🕕" (lemon-time--symbol 6 0))
  (string= "🕖" (lemon-time--symbol 7 0))
  (string= "🕗" (lemon-time--symbol 8 0))
  (string= "🕘" (lemon-time--symbol 9 0))
  (string= "🕙" (lemon-time--symbol 10 0))
  (string= "🕚" (lemon-time--symbol 11 0))
  (string= "🕛" (lemon-time--symbol 12 0))

  (string= "🕜" (lemon-time--symbol 1 30))
  (string= "🕝" (lemon-time--symbol 2 30))
  (string= "🕞" (lemon-time--symbol 3 30))
  (string= "🕟" (lemon-time--symbol 4 30))
  (string= "🕠" (lemon-time--symbol 5 30))
  (string= "🕡" (lemon-time--symbol 6 30))
  (string= "🕢" (lemon-time--symbol 7 30))
  (string= "🕣" (lemon-time--symbol 8 30))
  (string= "🕤" (lemon-time--symbol 9 30))
  (string= "🕥" (lemon-time--symbol 10 30))
  (string= "🕦" (lemon-time--symbol 11 30))
  (string= "🕜" (lemon-time--symbol 12 30)))

(provide 'lemon-time--test)
;;; lemon-time--test.el ends here
