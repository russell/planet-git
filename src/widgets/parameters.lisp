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

;;; From here down the code is taken from Hunchentoot

;;; Copyright (c) 2004-2010, Dr. Edmund Weitz.  All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:

;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.

;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.

;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.



(defun convert-parameter (argument type)
  "Converts the string ARGUMENT to TYPE where TYPE is one of the
symbols STRING, CHARACTERS, INTEGER, KEYWORD, or BOOLEAN - or
otherwise a function designator for a function of one argument.
ARGUMENT can also be NIL in which case this function also returns
NIL unconditionally."
  (when (listp argument)
    ;; this if for the case that ARGUMENT is NIL or the result of a
    ;; file upload
    (return-from convert-parameter argument))
  (case type
    (string argument)
    (character (and (= (length argument) 1)
                    (char argument 0)))
    (integer (ignore-errors (parse-integer argument :junk-allowed t)))
    (keyword (as-keyword argument :destructivep nil))
    (boolean t)
    (otherwise (funcall type argument))))

(defun compute-simple-parameter (parameter-name type parameter-reader)
  "Retrieves the parameter named PARAMETER-NAME using the reader
PARAMETER-READER and converts it to TYPE."
  (convert-parameter (funcall parameter-reader parameter-name) type))

(defun compute-list-parameter (parameter-name type parameters)
  "Retrieves all parameters from PARAMETERS which are named
PARAMETER-NAME, converts them to TYPE, and returns a list of
them."
  (loop for (name . value) in parameters
        when (string= name parameter-name)
        collect (convert-parameter value type)))

(defun compute-array-parameter (parameter-name type parameters)
  "Retrieves all parameters from PARAMETERS which are named like
\"PARAMETER-NAME[N]\" \(where N is a non-negative integer),
converts them to TYPE, and returns an array where the Nth element
is the corresponding value."
  ;; see <http://common-lisp.net/pipermail/tbnl-devel/2006-September/000660.html>
  #+:sbcl (declare (sb-ext:muffle-conditions warning))
  (let* ((index-value-list
          (loop for (full-name . value) in parameters
                for index = (register-groups-bind (name index-string)
                                ("^(.*)\\[(\\d+)\\]$" full-name)
                              (when (string= name parameter-name)
                                (parse-integer index-string)))
                when index
                collect (cons index (convert-parameter value type))))
         (array (make-array (1+ (reduce #'max index-value-list
                                        :key #'car
                                        :initial-value -1))
                            :initial-element nil)))
    (loop for (index . value) in index-value-list
          do (setf (aref array index) value))
    array))

(defun compute-hash-table-parameter (parameter-name type parameters key-type test-function)
  "Retrieves all parameters from PARAMETERS which are named like
\"PARAMETER-NAME{FOO}\" \(where FOO is any sequence of characters
not containing curly brackets), converts them to TYPE, and
returns a hash table with test function TEST-FUNCTION where the
corresponding value is associated with the key FOO \(converted to
KEY-TYPE)."
  (let ((hash-table (make-hash-table :test test-function)))
    (loop for (full-name . value) in parameters
          for key = (register-groups-bind (name key-string)
                        ("^(.*){([^{}]+)}$" full-name)
                      (when (string= name parameter-name)
                        (convert-parameter key-string key-type)))
          when key
          do (setf (gethash key hash-table)
                   (convert-parameter value type)))
    hash-table))

(defun compute-parameter (parameter-name parameter-type request-type)
  "Computes and returns the parameter\(s) called PARAMETER-NAME
and converts it/them according to the value of PARAMETER-TYPE.
REQUEST-TYPE is one of :GET, :POST, or :BOTH."
  (when (member parameter-type '(list array hash-table))
    (setq parameter-type (list parameter-type 'string)))
  (let ((parameter-reader (ecase request-type
                              (:get #'get-parameter)
                              (:post #'post-parameter)
                              (:both #'parameter)))
        (parameters (and (listp parameter-type)
                         (case request-type
                           (:get (get-parameters*))
                           (:post (post-parameters*))
                           (:both (append (get-parameters*)
					  (post-parameters*)))))))
    (cond ((atom parameter-type)
           (compute-simple-parameter parameter-name parameter-type parameter-reader))
          ((and (null (cddr parameter-type))
                (eq (first parameter-type) 'list))
           (compute-list-parameter parameter-name (second parameter-type) parameters))
          ((and (null (cddr parameter-type))
                (eq (first parameter-type) 'array))
           (compute-array-parameter parameter-name (second parameter-type) parameters))
          ((and (null (cddddr parameter-type))
                (eq (first parameter-type) 'hash-table))
           (compute-hash-table-parameter parameter-name (second parameter-type) parameters
                                         (or (third parameter-type) 'string)
                                         (or (fourth parameter-type) 'equal)))
          (t (parameter-error "Don't know what to do with parameter type ~S."
					  parameter-type)))))
