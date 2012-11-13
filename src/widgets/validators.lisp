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

(defparameter *validators* (make-hash-table)
  "A hash map of all the possible validators.")

(defun validate (fieldname value validators)
  (dolist (validator validators)
    (let ((validf (gethash validator *validators*)))
      (if validf
          (awhen (funcall validf fieldname value)
            (return it))
          (error "Error, can't find validator.")))))

(defun string-concat (&rest strings)
  (apply #'concatenate 'string strings))

(defmacro def-validator (name &body body)
  "Define a validator and add it to the list of available validators.
Within the body FIELDNAME and VALUE will be bound to variables and
available for use during the validation, errors messages should be
returned as a string."
  (let ((vname (gensym)))
    `(let ((,vname ,(intern (symbol-name name) "KEYWORD")))
       (when (gethash ,vname *validators*)
         (warn "Redefinition of validator ~S" ,vname))
       (setf (gethash ,vname *validators*)
             (lambda (fieldname value)
               (let ((fieldname (symbol-name fieldname)))
                   ,@body))))))

(def-validator required
  (when (= (length value) 0)
    (string-concat "Error, " fieldname " is required")))

(def-validator alpha-chars-only
  (when (car (list (scan "[^a-zA-Z]" value)))
    (string-concat "Error, " fieldname " can only contain alpha characters.")))

(def-validator alphanumeric-chars-only
  (when (car (list (scan "[^a-zA-Z0-9]" value)))
    (string-concat "Error, " fieldname " can only contain alphanumeric characters.")))

(def-validator email
  (unless (scan "^[^@]+@[^@]+[.][^@]+$" value)
    (string-concat "Error, " fieldname " is not a valid email address.")))

(def-validator ssh-key
  (unless (scan "^\\S*\\s+\\S*\\s+\\S*$" value)
    (string-concat "Error, " fieldname " is not a valid ssh-key.")))
