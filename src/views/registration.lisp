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

;;;; registration.lisp

(in-package #:planet-git)


(define-form-handler (register-page :uri "^/register$")
    ((new-user-form
      (fullname :parameter-type 'string :request-type :post
                :validate (#'validate-length))
      (username :parameter-type 'string :request-type :post
                :validate (#'validate-length
                           #'validate-username
                           #'validate-username-exists))
      (password :parameter-type 'string :request-type :post
                :validate (#'validate-length))
      (cpassword :parameter-type 'string :request-type :post
                 :validate (#'validate-password))
      (email :parameter-type 'string :request-type :post
             :validate (#'validate-length #'validate-email))))
  (if-valid-form
   (let ((login (create-user username fullname password email))
         (session (start-session)))
     (setf (session-value 'user session) login)
     (redirect (url-join (user-username login))))
   (render-standard-page (:title "Register")
     (form-fragment
      new-user-form
      (('fullname "Fullname:" "text")
       ('username "Username:" "text")
       ('email "Email:" "text")
       ('password "Password:" "password")
       ('cpassword "confirm passwd" "password"))
      :buttons ((:input :class "btn primary" :type "submit"
                        :name "new-user-form-submit" :value "Register"))))))


(defun login-widget (&key (login nil) (password nil) (came-from "/"))
  (let* ((errors (if (and login password) (validate-login)))
         (logged-in (when (and errors (= (hash-table-count errors) 0)) (login-session login password))))
    (if (and errors (gethash 'password errors))
        (setf (gethash 'password errors) "Invalid password.")
        (setf errors (make-hash-table)))
    (if logged-in
        (redirect came-from)
        (render-standard-page (:title "Login" :body-class "")
          (:form :action "" :class "login-form form-horizontal" :method "post"
                 (if (> (hash-table-count errors) 0)
                     (htm
                      (:div :class "alert alert-error"
                            (:button :class "close" :data-dismiss "alert" :type "button" "x")
                            (:strong "Error:") " found in the form."
                            )))
                 (:input :type "hidden" :name "came-from"
                         :value came-from)
                 (field-fragment "login" "Username or Email:" "text"
                                 :value login
                                 :error (gethash 'login errors))
                 (field-fragment "password" "Password:" "password"
                                 :error (gethash 'password errors))
                 (:div :class "form-actions"
                       (:button :class "btn btn-primary"
                               :type "submit"
                               :name "login"
                               :value "Login" "Login")
                       (:a :class "btn"
                                :href came-from "Cancel")))))))

(defgeneric login-page (method content-type))

(defmethod login-page ((method (eql :post)) (content-type (eql :html)))
  (with-request-args
      ((login :parameter-type 'string :request-type :post)
       (password :parameter-type 'string :request-type :post)
       (came-from :parameter-type 'string :init-form "/"))
    (login-widget :login login :password password :came-from came-from)))

(defmethod login-page ((method (eql :get)) (content-type (eql :html)))
  (with-request-args
      ((came-from :parameter-type 'string :init-form "/"))
    (login-widget :came-from came-from)))

(defgeneric logout-page (method content-type))

(defmethod logout-page ((method (eql :get)) (content-type (eql :html)))
  (logout-session)
  (redirect "/"))
