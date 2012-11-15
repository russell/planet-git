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

(defclass repository ()
  ((id :col-type serial :accessor id)
   (owner-id :col-type integer :initarg :owner-id)
   (name :col-type string :initarg :name :accessor repository-name)
   (path :col-type string :initarg :path :accessor repository-path)
   (branch :col-type (or db-null string)
           :initarg :branch
           :accessor repository-branch)
   (description :col-type (or db-null string)
                :initarg :description
                :accessor repository-description)
   (public :col-type boolean :initarg :public :accessor repository-public))
  (:metaclass dao-class)
  (:keys id))

(defun create-repository (name owner &optional (public nil))
  "Create a new repository with a NAME and an OWNER the repository
will not be PUBLIC by default by default."
  (let* ((username  (slot-value owner 'username))
         (relative-path (make-pathname :directory
                                       (list ':relative
                                             (string username)
                                             (string name))))
         (path (merge-pathnames relative-path
                                *git-user-homedir*)))
    ;; TODO this should check that there isn't a repository already
    (ensure-directories-exist path)
    (prog1
        (insert-dao
         (make-instance 'repository
                        :owner-id (slot-value owner 'id)
                        :name name
                        :path (namestring relative-path)
                        :public public))
      (ensure-git-repository-exist path t))))

(defun is-owner-p (user repository)
  "test if a user is the owner of a repository."
  (let ((user (coerce-user user)))
    (when (and repository user)
      (eql (slot-value repository 'owner-id)
             (id user)))))

(defun find-repository (owner name)
  (let ((user-id (id (coerce-user owner))))
    (when user-id
        (car (select-dao
              'repository (:and
                           (:= 'owner-id user-id)
                           (:= 'name name)))))))

(defgeneric has-permission-p (user thing permission))

(defmethod has-permission-p ((user login) (repository repository) (permission (eql :view)))
  (or (repository-public repository)
      (is-owner-p user repository)))

(defmethod has-permission-p ((user T) (repository repository) (permission (eql :view)))
  (slot-value repository 'public))

(defmethod url-for ((repository repository) (action (eql :get)) &key)
  (let ((user (get-dao 'login (slot-value repository 'owner-id))))
    (url-join (slot-value user 'username) (slot-value repository 'name))))

(defmethod url-for ((repository repository) (action (eql :new)) &key (username-or-user (loginp)))
  (let ((user (if (typep username-or-user 'login)
                  username-or-user
                  (car (select-dao 'login (:= 'username username-or-user))))))
    (url-join (slot-value user 'username) "new")))

(defgeneric repository-real-path (repo)
  (:method ((repo repository))
           (merge-pathnames (repository-path repo) *git-user-homedir*)))

(defun remove-ref-path (ref &optional (substring "refs/heads/"))
  "remove a substring from the start of a string"
  (let ((location (search substring ref)))
    (string-trim " "
		 (if location
		     (subseq ref (length substring))
		     ref))))

(defun selected-branch (repository repository-branches url-branch)
  "From a REPOSITORY orm object a list of the git REPOSTIORY-BRANCHES
and the possible branch in the url (URL-BRANCH) return the most
aproprate branch to display."
  (let ((default-branch (slot-value repository 'branch))
	(default-branch* "refs/heads/master"))
    (cond
      ((eq repository-branches nil)
       nil)
      ((find url-branch repository-branches :test #'equal)
       url-branch)
      ((find default-branch repository-branches :test #'equal)
       default-branch)
      ((find default-branch* repository-branches :test #'equal)
       default-branch*)
      (t
       (car repository-branches)))))
