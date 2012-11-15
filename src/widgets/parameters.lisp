;; Planet-Git a source code repository manager.
;; Copyright (C) 2011-2012 Russell Sim <russell.sim@gmail.com>
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


(in-package #:planet-git.widgets)

(defun compute-real-name (symbol)
  (string-downcase symbol))

(defun compute-real-form-name (symbol)
  (concatenate 'string (string-downcase symbol) "-submit"))

(defun compute-real-field-name (form field)
  (concatenate 'string (string-downcase form) "-" (string-downcase field)))

(defun compute-field-symbol (form field)
  (intern (concatenate 'string (symbol-name form) "-" (symbol-name field))))
