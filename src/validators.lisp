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


(in-package #:planet-git)

(defun validate-username-exists (fieldname username)
  (when (car (select-dao 'login (:= 'username username)))
    (concatenate 'string "Error, This " fieldname " is already taken.")))

(defun validate-email-exists (fieldname email)
  (when (car (select-dao 'email (:= 'email email)))
    (concatenate 'string "Error, This " fieldname " is already taken.")))


;;;; everything below this is deprecated


(defun validate-length (fieldname)
  (when (= (length (parameter fieldname)) 0)
    (concatenate 'string "Error, " fieldname " is required")))

(defun validate-username (fieldname)
  (when (car (list (scan "[^a-zA-Z]" (parameter fieldname))))
    (concatenate 'string "Error, " fieldname " can only contain alpha characters.")))

(defun validate-email (fieldname)
  (unless (scan "^[^@]+@[^@]+[.][^@]+$" (parameter fieldname))
    (concatenate 'string "Error, " fieldname " is not a valid email address.")))

(defun validate-key (fieldname)
  (unless (scan "^\\S*\\s+\\S*\\s+\\S*$" (parameter fieldname))
    (concatenate 'string "Error, " fieldname " is not a valid ssh-key.")))

(defun validate-password (fieldname)
  (when (equal (parameter fieldname)
	       (parameter 'password))
    (concatenate 'string "Error, passwords doesn't match.")))

(defmacro validate-field (fieldname errors &rest validators)
  `(let ((lname ,fieldname)
         (lerrors ,errors))
     (loop
        :for x :in (list ,@validators)
        :until (gethash lname lerrors)
        :do (let ((validation-error (funcall x (string-downcase (string lname)))))
              (unless (= (length validation-error) 0)
                (setf (gethash lname lerrors) validation-error))))))


(defmacro def-validator (name () &body body)
  `(defun ,name ()
     (let ((errors (make-hash-table)))
       (if (eq (request-method*) :post)
	   (progn
	     ,@body))
       errors)))

(def-validator validate-newrepository ()
  (validate-field 'name errors #'validate-length))
