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

(defclass login ()
  ((id :col-type serial
       :accessor id)
   (fullname :col-type string
             :initarg :fullname
             :accessor user-fullname)
   (location :col-type (or db-null string)
             :initarg :location
             :accessor user-location)
   (username :col-type string
             :initarg :username
             :accessor user-username)
   (password :col-type string
             :initarg :password
             :accessor user-password))
  (:metaclass dao-class)
  (:keys id))

(defclass email ()
  ((id :col-type serial
       :accessor id)
   (user-id :col-type integer
            :initarg :user-id
            :accessor email-user-id)
   (email :col-type string
          :initarg :email
          :accessor email-address)
   (primary :col-type boolean
            :initform nil
            :initarg :primary
            :accessor email-primary)
   (verified :col-type boolean
             :initform nil
             :initarg :verified
             :accessor email-verified))
  (:metaclass dao-class)
  (:keys id))

(defclass key ()
  ((id :col-type serial
       :accessor id)
   (user-id :col-type integer
            :initarg :user-id
            :accessor key-user-id)
   (title :col-type string
          :initarg :title
          :accessor key-title)
   (type :col-type string
         :initarg :type
         :accessor key-type)
   (key :col-type string
        :initarg :key
        :accessor key-value))
  (:metaclass dao-class)
  (:keys id))

(defgeneric url-for (item action &key &allow-other-keys))

(defmethod url-for ((email email) (action (eql :delete)) &key (user (loginp)))
  (url-join (user-username user) "settings" "email" (write-to-string (id email)) "delete"))

(defmethod url-for ((key key) (action (eql :delete)) &key (user (loginp)))
  (url-join (user-username user) "settings" "key" (write-to-string (id key)) "delete"))

(defmethod url-for ((user login) (action (eql :settings)) &key)
  (url-join (user-username user) "settings"))

(defmethod url-for ((user login) (action (eql :home)) &key)
  (url-join (user-username user)))

(defun create-user (username fullname password email)
  "Create a new user from the attributes USERNAME FULLNAME PASSWORD
and set the primary email address to EMAIL"
  (with-transaction ()
    (let ((login (insert-dao
                  (make-instance 'login
                                 :fullname fullname
                                 :username username
                                 :password password))))
      (insert-dao
       (make-instance 'email
                      :user-id (id login)
                      :email email
                      :primary t))
      login)))

(defgeneric user-primary-email (user)
  (:method ((user login))
           (car (select-dao 'email (:and (:= 'user-id (id user))
                                         (:= 'primary t))))))

(defmethod encode-json ((user login)
                        &optional (stream *json-output*))
  "Write the JSON representation of the user to STREAM (or to
   (branch :col-type (or db-n*JSON-OUTPUT*)."
  (with-object (stream)
    (encode-object-member 'username (user-username user) stream)
    (encode-object-member 'fullname (user-fullname user) stream)
    (encode-object-member 'location (coalesce (user-location user)) stream)
    (encode-object-member 'photo (user-gravatar-url user) stream)))

(defun gravatar-url (email &key (size 80))
  "Return the gravatar url for an EMAIL address, an optional SIZE
keyword can be used to set the requested size."
    (concatenate 'string
		 "https://secure.gravatar.com/avatar/"
		 (format nil "~(~{~2,'0X~}~)"
			 (map 'list #'identity (md5sum-sequence (coerce email 'simple-string))))
		 "?s="
		 (prin1-to-string size)))

(defgeneric user-gravatar-url (user &key size))

(defmethod user-gravatar-url ((user login) &key (size 80))
  "Return the url to a USER's gravatar, an optional SIZE keyword can
be used to set the requested size."
  (gravatar-url (email-address (user-primary-email user)) :size size))

(defmethod key-to-authorizedkeys ((key key))
  "Add the users authorized KEY to the authorized_keys file.  If the
directory doesn't exist then create it."
  (let ((authorizedkeys-file (merge-pathnames "authorized_keys"
                                              *git-user-sshdir*)))
    (unless (directory-exists-p *git-user-sshdir*)
      (ensure-directories-exist *git-user-sshdir*)
      (sb-posix:chmod *git-user-sshdir*
                      (logior sb-posix::s-iread sb-posix::s-iwrite
                              sb-posix::s-iexec))
      (with-open-file (stream
                       authorizedkeys-file
                       :direction :output :if-does-not-exist :create))
      (sb-posix:chmod authorizedkeys-file
                      (logior sb-posix::s-iread sb-posix::s-iwrite)))
  (with-open-file (stream
                   authorizedkeys-file
                   :direction :output :if-exists :append)
    (format stream
            "command=\"KEY_ID=~A ~A\",no-port-forwarding,no-agent-forwarding,no-X11-forwarding,no-pty ~A ~A ~A~%"
            (id key) *git-shell-path* (key-type key) (key-value key) (key-title key)))))

(defun key-parse (key)
  "Parse a KEY string and return a new KEYS instance, if there is a
  current user then set them as the foreign key."
  (register-groups-bind
      (type key title)
      ("^(\\S*)\\s+(\\S*)\\s+(\\S*)$" key)
    (eval
     `(make-instance 'key :type ,type :key ,key :title ,title
                     ,@(when (loginp) (list :user-id (id (loginp))))))))
