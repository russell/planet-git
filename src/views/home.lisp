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

;;;; home.lisp

(in-package #:planet-git)

(def-who-macro user-item-fragment (user)
  "Create a users description for the front page"
  `(htm
    (:div :class "well user"
	  (:a :href (str (url-join (slot-value ,user 'username)))
          (:img :src (user-gravatar-url ,user :size 40))
	      (:h3 :class "name"
		   (str (slot-value ,user 'username)))))))

(defun list-users (&optional (offset 0) (number *max-results-per-query*))
  (with-request-args
      ((offset :parameter-type 'string :request-type :get :init-form offset))
    (let ((users (query-dao 'login
                            (:limit (:select 'login.* :from 'login)
                                    number
                                    offset))))
      users)))

(defgeneric home-page (method content-type))

(defmethod home-page ((method (eql :get)) (content-type (eql :html)))
  (let ((users (list-users)))
    (render-standard-page (:title "Planet Git"
                           :subtitle "a bad clone of github or gitorious.")
      (loop :for user :in users
            :do (user-item-fragment user)))))


(defmethod home-page ((method (eql :get)) (content-type (eql :json)))
  (let ((users (list-users)))
   (json-output-to-string
     (with-object ()
       (encode-object-member 'count (count-dao 'login))
       (encode-object-member 'next nil)
       (encode-object-member 'previous nil)
       (as-object-member ('results)
         (with-array ()
           (loop :for user :in users
                 :do (as-array-member ()
                       (encode-json user)))))))))
