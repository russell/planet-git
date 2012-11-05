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
    (insert-dao
     (make-instance 'repository
                    :owner-id (slot-value owner 'id)
                    :name name
                    :path (namestring relative-path)
                    :public public))
    (ensure-repository-exist path t)))

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
