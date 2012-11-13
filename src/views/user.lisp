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
                (t
                 (car (select-dao 'login (:= 'username username-or-user)))))))
    (when (and current-user user
               (eql (slot-value user 'id)
                      (slot-value current-user 'id))))))

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


(def-who-macro* email-item-fragment (user email)
  "this fragment renders a users email address as a list item with a
delete button"
  (htm
   (:tr
    (:td
	 (:a :class "close"
         :href (str
                (url-join (user-username user)
                          "settings"
                          "email"
                          (write-to-string (id email))
                          "delete"))
	     (str "x"))
	 (str (email-address email))))))

(def-who-macro* key-item-fragment (user key)
  "this fragment renders a users email address as a list item with a
delete button"
  (htm
   (:tr
    (:td
	 (:a :class "close"
         :href (str
                (url-join (user-username user)
                          "settings"
                          "key"
                          (write-to-string (id key))
                          "delete"))
	     (str "x"))
	 (str (key-title key))))))


(def-who-macro* user-settings-tmpl (user emails keys)
  (render-standard-page (:title (str (user-username user))
                         :page-header
                         ((:img :src (user-gravatar-url user :size 40))
                          (:h1 (:a :href (url-join (user-username user))
                                   (str (user-username user)))
                               (:small "Settings"))))
    (widget-tabs
     ("Personal"
      (:h2 "Personal Information")
      (form-fragment login-form
                     (('fullname "Fullname:" "text" :value (user-fullname user)))
                     :class "well form-stacked"
                     :buttons ((:input :type "submit"
                                       :class "btn primary"
                                       :name "login-form-submit"
                                       :value "Save")))
      (:h2 "Emails")
      (:table :class "table"
              (labels ((email-fragment (emails)
                         (let* ((email (car emails)) (rest (cdr emails)))
                           (email-item-fragment user email)
                           (when rest (email-fragment rest)))))
                (when emails (email-fragment emails))))
      (form-fragment email-form
                     (('email "Email:" "text"))
                     :class "well form-inline"
                     :buttons ((:input :type "submit"
                                       :class "btn primary"
                                       :name "email-form-submit"
                                       :value "Add"))))
     ("Keys"
      (:h2 "Keys")
      (:table :class "table"
              (labels ((key-fragment (keys)
                         (let* ((key (car keys)) (rest (cdr keys)))
                           (key-item-fragment user key)
                           (when rest (key-fragment rest)))))
                (when keys (key-fragment keys))))
      (form-fragment key-form
                     (('key "Key:" "textarea" :class "input-xlarge" :rows 3))
                     :class "well form-stacked"
                     :buttons ((:input :type "submit"
                                       :class "btn primary"
                                       :name "key-form-submit"
                                       :value "Save")))))))


(defclass email-form (form)
  ((email :parameter-type string
          :request-type :post
          :initform ""
          :type :email
          :validators (:required :email)))
  (:metaclass form-class))

(defmethod save-form ((form key-form))
  (insert-dao
   (make-instance 'email
                  :user-id (slot-value (loginp) 'id)
                  :email (slot-valke form 'email))))

(defclass key-form (form)
  ((key :parameter-type string
          :request-type :post
          :initform ""
          :type :text-area
          :validators (:required :ssh-key)))
  (:metaclass form-class))

(defmethod save-form ((form key-form))
  (insert-dao (key-parse (slot-value form 'key))))

(defclass user-form (form)
  ((fullname :parameter-type string
          :request-type :post
          :initform ""
          :type :text-area
          :validators (:required)))
  (:metaclass form-class))

(defmethod save-form ((form key-form))
  (let ((user (get-dao 'login (slot-value (loginp) 'id))))
    (setf (slot-value user 'fullname) (slot-value form 'fullname))
    (update-dao user)))

(defgeneric user-settings-page (method content-type &key username))

(defmethod user-settings-page ((method (eql :get)) (content-type (eql :html)) &key username)
  (let* ((user-form (make-instance 'user-form))
         (email-form (make-instance 'email-form :submit-action "Add"))
         (key-form (make-instance 'key-form :submit-action "Add"))
         (user (car (select-dao 'login (:= 'username username))))
         (is-current-user (is-current-user user)))
    (if is-current-user
        (progn
          (cond
            ((validate-form (parse-form email-form))
             (save-form email-form))
            ((validate-form (parse-form user-form))
             (save-form user-form))
            ((validate-form (parse-form key-form))
             (save-form key-form)))
          (let ((emails (select-dao 'email (:= 'user-id (id user))))
                (keys (select-dao 'key (:= 'user-id (id user)))))
            (user-settings-tmpl user emails keys)))
        (setf (return-code*) +http-forbidden+))))


(define-rest-handler (user-email-delete :uri "^/(\\w+)/settings/email/(\\w+)/delete/?$" :args (username email-id)) ()
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
	  (redirect (url-join (user-username user) "settings")))
	(setf (return-code*) +http-forbidden+))))

(define-rest-handler (user-key-delete :uri "^/(\\w+)/settings/key/(\\w+)/delete/?$" :args (username key-id)) ()
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
	  (redirect (url-join (user-username user) "settings")))
	(setf (return-code*) +http-forbidden+))))


(define-rest-handler (add-ssh-key
                      :uri "^/(\\w+)/settings/add-key?$"
                      :args (username))
    ()
  (let*
      ((user (car (select-dao 'login (:= 'username username))))
       (is-current-user (is-current-user user)))
    ;; what the heck happened here... must have lost interest...
    ))
