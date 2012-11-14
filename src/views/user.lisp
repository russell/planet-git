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

;;;; user.lisp

(in-package #:planet-git)

(defun is-current-user (username-or-user)
  (let ((current-user (loginp))
        (user (cond
                ((typep username-or-user 'login)
                 username-or-user)
                ((null username-or-user)
                 nil)
                (t
                 (car (select-dao 'login (:= 'username username-or-user)))))))
    (when (and current-user user
               (eql (slot-value user 'id)
                      (slot-value current-user 'id)))
      current-user)))

(def-who-macro repository-item-fragment (name owner public)
  `(htm
    (:div :class "well project"
          (:a :href (str (url-join ,owner ,name))
              (:h3 :class "name"
                   (str ,name)))
          (unless ,public
            (htm (:span :class "label label-important" "Private"))))))

(defgeneric user-page (method content-type &key username))

(defmethod user-page ((method (eql :get)) (content-type (eql :html)) &key username)
  (let ((user (car (select-dao 'login (:= 'username username)))))
    (if user
        (let ((username (slot-value user 'username))
              (is-current-user (equal (slot-value user 'username)
                                      (when (loginp) (slot-value (loginp) 'username)))))
          (render-user-page
              (user
               :extra-header (when is-current-user
                               (htm (:a :class "btn primary pull-right"
                                        :href "/repository/new"
                                        "Add Repository"))))
            (let ((repositories (select-dao
                                    'repository (:= 'owner-id (slot-value user 'id)))))
              (labels ((repository-fragment (repos)
                         (let* ((repo (car repos)) (rest (cdr repos))
                                (visible (or (slot-value repo 'public)
                                             (equal (slot-value user 'username)
                                                    (when (loginp) (slot-value (loginp) 'username)))))
                                (public (repository-public repo)))
                           (when (and repo (or visible is-current-user))
                             (repository-item-fragment (slot-value repo 'name)
                                                       username
                                                       public))
                           (when rest (repository-fragment rest)))))
                (when repositories (repository-fragment repositories))))))
        (setf (return-code*) +http-not-found+))))


(defmethod email-item-fragment ((email email))
  "this fragment renders a users email address as a list item with a
delete button"
  (let ((url (url-for email :delete)))
    (with-html-output (*standard-output* nil)
      (htm
       (:tr
        (:td
         (:a :class "close" :href url (str "x"))
         (str (email-address email))))))))

(defmethod key-item-fragment ((key key))
  "this fragment renders a users email address as a list item with a
delete button"
  (let ((url (url-for key :delete)))
    (with-html-output (*standard-output* nil)
      (htm
       (:tr
        (:td
         (:a :class "close" :href url (str "x"))
         (str (key-title key))))))))


(def-who-macro* user-settings-tmpl (user emails keys user-form email-form key-form)
  (render-standard-page (:title (str (user-username user))
                         :page-header
                         ((:img :src (user-gravatar-url user :size 40))
                          (:h1 (:a :href (url-for user :home)
                                   (str (user-username user)))
                               (:small "Settings"))))
    (widget-tabs
     ("Personal"
      (:h2 "Personal Information")
      (render-widget user-form)
      (:h2 "Emails")
      (:table :class "table"
              (dolist (email emails)
                (email-item-fragment email)))
      (render-widget email-form))
     ("Keys"
      (:h2 "Keys")
      (:table :class "table"
              (dolist (key keys)
                (key-item-fragment key)))
      (render-widget key-form)))))


(defclass email-form (form)
  ((email :parameter-type string
          :request-type :post
          :initform ""
          :type :email
          :validators (:required :email)))
  (:metaclass form-class))

(defmethod save-form ((form email-form))
  "save form then reset it's content."
  (insert-dao
   (make-instance 'email
                  :user-id (slot-value (loginp) 'id)
                  :email (slot-value form 'email)))
  (setf (slot-value form 'email) ""))

(defclass key-form (form)
  ((key :parameter-type string
        :request-type :post
        :initform ""
        :type :text-area
        :validators (:required :ssh-key)))
  (:metaclass form-class))

(defmethod save-form ((form key-form))
  "save form then reset it's content."
  (insert-dao (key-parse (slot-value form 'key)))
  (setf (slot-value form 'key) ""))

(defclass user-form (form)
  ((fullname :parameter-type string
             :request-type :post
             :initarg :fullname
             :initform ""
             :type :text
             :validators (:required)))
  (:metaclass form-class))

(defmethod save-form ((form user-form))
  "save form then reset it's content."
  (let ((user (get-dao 'login (slot-value (loginp) 'id))))
    (setf (slot-value user 'fullname) (slot-value form 'fullname))
    (setf (slot-value form 'fullname) "")
    (update-dao user)))


(defgeneric user-settings-page (method content-type &key username))

(defmethod user-settings-page ((method (eql :get)) (content-type (eql :html)) &key username)
  (let* ((user (car (select-dao 'login (:= 'username username))))
         (is-current-user (is-current-user user)))
    (if is-current-user
        (let ((emails (select-dao 'email (:= 'user-id (id user))))
              (keys (select-dao 'key (:= 'user-id (id user))))
              (user-form (make-instance 'user-form :fullname (slot-value user 'fullname)))
              (email-form (make-instance 'email-form :submit-action "Add"))
              (key-form (make-instance 'key-form :submit-action "Add")))
          (user-settings-tmpl user emails keys user-form email-form key-form))
        (setf (return-code*) +http-forbidden+))))

(defmethod user-settings-page ((method (eql :post)) (content-type (eql :html)) &key username)
  (let* ((user (car (select-dao 'login (:= 'username username))))
         (is-current-user (is-current-user user)))
    (if is-current-user
        (let ((user-form (make-instance 'user-form :fullname (slot-value user 'fullname)))
              (email-form (make-instance 'email-form :submit-action "Add"))
              (key-form (make-instance 'key-form :submit-action "Add")))
          (cond
            ((validate-form (parse-form email-form))
             (save-form email-form))
            ((validate-form (parse-form user-form))
             (save-form user-form))
            ((validate-form (parse-form key-form))
             (save-form key-form)))
          (let ((emails (select-dao 'email (:= 'user-id (id user))))
                (keys (select-dao 'key (:= 'user-id (id user)))))
            (user-settings-tmpl user emails keys user-form email-form key-form)))
        (setf (return-code*) +http-forbidden+))))


(defmethod email-delete-page ((method (eql :get)) (content-type T) &key username email-id)
  (let*
      ((user (car (select-dao 'login (:= 'username username))))
       (is-current-user (is-current-user user)))
    (if is-current-user
        (let ((email (car
                      (select-dao 'email
                          (:and (:= 'id email-id) (:= 'user-id (id user)))))))
          (if email
              (delete-dao email)
              (setf (return-code*) +http-not-found+))
          (redirect (url-for user :settings)))
        (setf (return-code*) +http-forbidden+))))

(defmethod key-delete-page ((method (eql :get)) (content-type T) &key username key-id)
  (let*
      ((user (car (select-dao 'login (:= 'username username))))
       (is-current-user (is-current-user user)))
    (if is-current-user
	(let ((key (car
		      (select-dao 'key
					     (:and (:= 'id key-id) (:= 'user-id (id user)))))))
	  (if key
	      (delete-dao key)
	      (setf (return-code*) +http-not-found+))
	  (redirect (url-for user :settings)))
	(setf (return-code*) +http-forbidden+))))


(define-rest-handler (add-ssh-key
                      :uri "^/(\\w+)/settings/add-key?$"
                      :args (username))
    ()
  (let* ((user (car (select-dao 'login (:= 'username username))))
         (is-current-user (is-current-user user)))
    (if is-current-user
        (let ((key (car
                    (select-dao 'key
                        (:and (:= 'id key-id) (:= 'user-id (id user)))))))
          (if key
              (delete-dao key)
              (setf (return-code*) +http-not-found+))
          (redirect (url-for user :settings)))
        (setf (return-code*) +http-forbidden+))
    ))
