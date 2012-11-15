;; Planet-Git a source code repository manager.
;; Copyright (C) 2012 Russell Sim <russell.sim@gmail.com>
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

(defun 404-page (&rest rest)
  (declare (ignore rest))
  (setf (return-code*) +http-not-found+))

(defparameter *traversal-path* nil)

(setq *traversal-path*
      '(nil home-page
        ("login" login-page)
        ("logout" logout-page)
        ("register" register-page)
        (:username user-page
         ("new" new-repository-page)
         ("settings" user-settings-page
          ("email" 404-page
           (:email-id 404-page
            ("delete" email-delete-page)))
          ("key" 404-page
           (:key-id 404-page
            ("delete" key-delete-page)))))
        ;; (:repository repository-home-page
        ;;  ("key" repository-key-access)
        ;;  ("branch" repository-branch-page)
        ;;  ("commits" repository-commits))
        ))

(defvar *content-type-list*
  '(("text/html" :html)
    ("application/json" :json)))

(defun content-type-to-symbol (content-type)
  (cadr (assoc content-type *content-type-list* :test #'equal)))

(defun request-accepts-type (request)
  (dolist (ct (request-accepts request))
    (awhen (content-type-to-symbol ct)
      (return it))))


(defun traverse-path (path &optional (tree *traversal-path*))
  (let (interesting-parts)
    (labels ((walk-uri (sub-uri sub-tree)
               (destructuring-bind (segment func &rest sub-tree1) sub-tree
                 (let ((uri-segment (car sub-uri))
                       (uri-rest (cdr sub-uri)))
                   (when (or (null segment) (string-equal segment uri-segment) (symbolp segment))
                     (when (and (symbolp segment) (not (null segment)))
                       (setf interesting-parts `(,segment ,uri-segment ,@interesting-parts)))
                     (aif (cond
                            ((null uri-rest)
                             func)
                            ((not (equal segment uri-segment))
                             nil))
                          it
                          (progn
                            (dolist (branch sub-tree1)
                              (awhen (walk-uri uri-rest branch)
                                (return it))))))))))
      (destructuring-bind (segment func &rest sub-tree) tree
        (if (and (null segment) (null path))
            (list func)
            (dolist (branch sub-tree)
              (awhen (walk-uri path branch)
                (return (cons it interesting-parts)))))))))


(defun dispatch-traverser-handlers (request)
  (let ((path (remove-if (lambda (s) (string-equal s ""))
                         (split-sequence #\/ (script-name request)))))
    (traverse-path path)))


(defclass traverser-acceptor (acceptor)
  ()
  (:documentation "This is the acceptor of the ``traverser'' extension
  to the Hunchentoot framework."))

(defmethod acceptor-dispatch-request ((acceptor traverser-acceptor) request)
  "The easy request dispatcher which selects a request handler
based on a list of individual request dispatchers all of which can
either return a handler or neglect by returning NIL."
  (loop :for dispatcher :in *dispatch-table*
        :for action = (funcall dispatcher request)
        :when action
          :return
          (progn
            (if (listp action)
                (progn
                  (let ((content-type (or (request-accepts-type request)
                                          (content-type-to-symbol *default-content-type*)))
                        (method (request-method request)))
                    (apply (car action) (cons method (cons content-type (cdr action))))))
                (progn
                  (funcall action))))
        :finally (call-next-method)))
