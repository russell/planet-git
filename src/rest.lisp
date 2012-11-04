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

;;;; rest.lisp

(in-package #:planet-git)

;; this module provides macros for rest like handlers


(defmethod request-accepts ((request request))
  (let* ((header (cl-ppcre:split ";" (header-in "accept" request)))
         (content-types (cl-ppcre:split "," (car header)))
         (options (cdr header)))
    content-types))

;;text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8
(defun request-accepts* (&optional (request *request*))
  (request-accepts request))

(defparameter *rest-handler-alist* nil
  "An alist of \(URI acceptor-names function) lists defined by
DEFINE-REST-HANDLER.")

(defmacro define-rest-handler (description lambda-list &body body)
  (when (atom description)
    (setf description (list description)))
  (destructuring-bind (name &key uri args (acceptor-names t)
                              (content-type "text/html")
                              (default-parameter-type ''string)
                              (default-request-type :both))
      description
    `(progn
       ,@(when uri
           (list
            (with-rebinding (uri)
              `(progn
                 (setf *rest-handler-alist*
                       (substitute-if
                        (list ,uri ,acceptor-names ,content-type ',name)
                        (lambda (list)
                          (and (or (equal ,uri (first list))
                                   (eq ',name (third list)))
                               (or (eq ,acceptor-names t)
                                   (intersection ,acceptor-names
                                                 (second list)))))
                        *rest-handler-alist*))))))
       (defun ,name (&key ,@(loop for part in lambda-list
                                  collect (make-defun-parameter part
                                                                default-parameter-type
                                                                default-request-type)))
         ,(if args
              `(register-groups-bind ,args
                   (,uri (script-name*))
                 ,@body)
              `(progn ,@body))))))

(defun dispatch-rest-handlers (request)
  "This is a dispatcher which returns the appropriate handler
defined with DEFINE-REST-HANDLER, if there is one."
  (loop
     :for (uri acceptor-names content-type rest-handler)
     :in *rest-handler-alist*
     :when (and (or (eq acceptor-names t)
                    (find (acceptor-name *acceptor*) acceptor-names :test #'eq))
                (cond ((stringp uri)
                       (let ((scanner (create-scanner uri)))
                         (scan scanner (script-name request))))
                      (t (funcall uri request)))
                (find content-type (request-accepts*) :test #'equal))
     :do (progn
           (log-message* *lisp-warnings-log-level* "Request Handler: ~s" rest-handler)
           (return rest-handler))))
