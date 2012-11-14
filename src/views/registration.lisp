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

;;
;; Registration form and page.
;;

(defclass register-form (form)
  ((fullname :parameter-type string
             :request-type :post
             :initform ""
             :validators (:required))
   (username :parameter-type string
             :request-type :post
             :initform ""
             :validators (:required :alpha-chars-only)) ;; check that the username isn't taken
   (password :parameter-type string
             :type :password
             :initform ""
             :request-type :post
             :validators (:required))
   (confirm-password :parameter-type string
                     :request-type :post
                     :initform ""
                     :type :password
                     :label "Confirm Password:"
                     :validators (:required))
   (email :parameter-type string
          :request-type :post
          :initform ""
          :type :email
          :validators (:required :email)))
  (:metaclass form-class))

(defmethod validate-form ((form register-form))
  (call-next-method)
  (when (valid-p form)
    (unless (string-equal (slot-value form 'password)
                          (slot-value form 'confirm-password))
      (setf (gethash 'confirm-password (form-errors form))
            "Error, passwords do not match."))
    (awhen (validate-username-exists "Username" (slot-value form 'username))
      (setf (gethash 'username (form-errors form)) it))
    (awhen (validate-email-exists "Email" (slot-value form 'email))
      (setf (gethash 'email (form-errors form)) it)))
  (valid-p form))

(defmethod save-form ((form register-form))
  "Save the form and return the created user."
  (with-slots (fullname username password email)
      form
    (create-user username fullname password email)))

(defgeneric register-page (method content-type))

(defmethod register-page ((method (eql :post)) (content-type (eql :html)))
  (let ((form (make-instance 'register-form :submit-action "Register")))
    (parse-form form)
    (if (validate-form form)
        (progn
          (session-attach-user (save-form form))
          (redirect "/"))
        (render-standard-page (:title "Register")
          (render-widget form)))))

(defmethod register-page ((method (eql :get)) (content-type (eql :html)))
  (let ((form (make-instance 'register-form :submit-action "Register")))
    (parse-form form)
    (render-standard-page (:title "Register")
      (render-widget form))))


;;
;; Login Form and Page
;;

(defclass login-form (form)
  ((login :parameter-type string
          :request-type :post
          :validators (:required)
          :initform ""
          :label "Username or Email:")
   (password :parameter-type string
             :request-type :post
             :initform ""
             :validators (:required)
             :type :password)
   (came-from :parameter-type string
              :initform (let ((from (or (and (boundp '*request*) (request-uri*)) "/")))
                          (if (string-equal from "/login")
                              "/"
                              from))
              :type :hidden))
  (:metaclass form-class))

(defmethod validate-form ((form login-form))
  (call-next-method)
  (when (valid-p form)
    (unless (login-session (slot-value form 'login) (slot-value form 'password))
      (setf (gethash 'password (form-errors form))
            "Error, invalid username or password.")))
  (valid-p form))

(defmethod render-widget ((form login-form))
  (form-widget form :class "login-form form-horizontal"))

(defmethod render-buttons ((form login-form))
  (with-html-output (*standard-output* nil)
    (htm
     (:div :class "form-actions"
           (:button :class "btn btn-primary"
                    :type "submit"
                    :name (form-real-name form)
                    :value "login"
                    "Login")
           (:a :class "btn" :href (str (slot-value form 'planet-git::came-from)) "Cancel")))))

(defgeneric login-page (method content-type))

(defmethod login-page ((method (eql :post)) (content-type (eql :html)))
  (let ((login-form (make-instance 'login-form)))
    (parse-form login-form)
    (if (validate-form login-form)
        (redirect (slot-value login-form 'came-from))
        (render-standard-page (:title "Login")
          (render-widget login-form)))))

(defmethod login-page ((method (eql :get)) (content-type (eql :html)))
  (let ((login-form (make-instance 'login-form)))
    (render-standard-page (:title "Login")
      (render-widget login-form))))


;;
;; Logout Page
;;

(defgeneric logout-page (method content-type))

(defmethod logout-page ((method (eql :get)) (content-type (eql :html)))
  (logout-session)
  (redirect "/"))
