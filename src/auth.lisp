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

(defun compare-password-hash (passwordhash password)
  "This is boiler plate that will eventually compare the encrypted
passwords"
  (if (string= passwordhash password)
      T
      nil))

(defun verify-password (login password)
  "Confirm that the `password' is correct for a user with the
`login'.  Return the user object of the authenticated user."
  (let ((user
         (car (query-dao
               'login
               (:select 'login.*
                        :from 'login
                        :left-join 'email :on (:= 'login.id 'email.user-id)
                        :where (:or (:= 'login.username login)
                                    (:= 'email.email login)))))))
    (if (compare-password-hash (user-password user) password)
        user
        nil)))


(defun login-session (login password)
  "Log a user in to a session, the user object will be stored as the
value of the session."
  (let ((user (verify-password login password)))
    (if user
        (let ((session (start-session)))
          (setf (session-value 'user session) user))
        nil)))


(defun logout-session ()
  "Remove the user from the current session-login"
  (remove-session *session*))


(defun loginp ()
  "If there is a current session then reurn its value which will be a
user object."
  (when (boundp '*session*)
    (session-value 'user)))
