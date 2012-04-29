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

;;;; planet-git.lisp

(in-package #:planet-git)

;;; "planet-git" goes here. Hacks and glory await!
;(use-package '(swank hunchentoot cl-who postmodern))

(require :swank)
(require :hunchentoot)
(require :cl-who)
(require :postmodern)
(require :cl-ppcre)


;;; Global Config

(defparameter *repository-directory* #p"/home/russell/tmp/planet-git/")

;;; Webserver

(defun resource-path (path)
  "looks up path relative to whereever this asdf system is installed.  Returns a truename"
  (truename (asdf:system-relative-pathname :planet-git path)))


(setq hunchentoot:*dispatch-table*
 (list
  'hunchentoot:dispatch-easy-handlers
  'dispatch-rest-handlers
  (hunchentoot:create-regex-dispatcher "^/?$" 'home-page)
  (hunchentoot:create-regex-dispatcher "^/[^/]+/$" 'user-page)
  (hunchentoot:create-regex-dispatcher "^/[^/]+/[^/]+/$" 'repository-home-page)
  (hunchentoot:create-regex-dispatcher "^/[^/]+/[^/]+/branch/[^/]+/$" 'repository-branch-page)
  (hunchentoot:create-regex-dispatcher "^/[^/]+/[^/]+/key/[^/]+/$" 'repository-key-page)
  (hunchentoot:create-folder-dispatcher-and-handler "/static/" (resource-path "static"))
  ))


;;; Database
(defclass login ()
  ((id :col-type serial :accessor id)
   (fullname :col-type string :initarg :fullname)
   (username :col-type string :initarg :username)
   (email :col-type string :initarg :email)
   (password :col-type string :initarg :password))
  (:metaclass postmodern:dao-class)
  (:keys id))

(defclass email ()
  ((id :col-type serial :accessor id)
   (user-id :col-type integer :initarg :user-id)
   (email :col-type string :initarg :email))
  (:metaclass postmodern:dao-class)
  (:keys id user-id))

(defclass keys ()
  ((id :col-type serial :accessor id)
   (user-id :col-type integer :initarg :user-id)
   (title :col-type string :initarg :title)
   (key :col-type string :initarg :email))
  (:metaclass postmodern:dao-class)
  (:keys id user-id))

(defclass repository ()
  ((id :col-type serial :accessor id)
   (owner-id :col-type integer :initarg :owner-id)
   (name :col-type string :initarg :name)
   (path :col-type string :initarg :path)
   (branch :col-type (or postmodern:db-null string) :initarg :branch)
   (public :col-type boolean :initarg :public))
  (:metaclass postmodern:dao-class)
  (:keys id))


(defun create-tables ()
  (unless (postmodern:table-exists-p 'login)
    (postmodern:execute (postmodern:dao-table-definition 'login)))
  (unless (postmodern:table-exists-p 'email)
    (postmodern:execute (postmodern:dao-table-definition 'email)))
  (unless (postmodern:table-exists-p 'keys)
    (postmodern:execute (postmodern:dao-table-definition 'keys)))
  (unless (postmodern:table-exists-p 'repository)
    (postmodern:execute (postmodern:dao-table-definition 'repository))))


;;; Validation


(defun validate-length (fieldname)
  (when (= (length (hunchentoot:parameter fieldname)) 0)
    (concatenate 'string "Error, " fieldname " is required")))

(defun validate-username (fieldname)
  (when (car (list (cl-ppcre:scan "[^a-zA-Z]" (hunchentoot:parameter fieldname))))
    (concatenate 'string "Error, " fieldname " can only contain alpha characters.")))

(defun validate-username-exists (fieldname)
  (when (car
	 (postmodern:select-dao 'login
				(:= 'username
				    (hunchentoot:parameter fieldname))))
    (concatenate 'string "Error, This " fieldname " is already taken.")))

(defun validate-email-exists (fieldname)
  (when (car
	 (postmodern:select-dao 'email
				(:= 'email
				    (hunchentoot:parameter fieldname))))
    (concatenate 'string "Error, This " fieldname " is already taken.")))

(defun validate-email (fieldname)
  (unless (cl-ppcre:scan "^[^@]+@[^@]+[.][^@]+$" (hunchentoot:parameter fieldname))
    (concatenate 'string "Error, " fieldname " is not a valid email address.")))

(defun validate-password (fieldname)
  (when (equal (hunchentoot:parameter fieldname)
	       (hunchentoot:parameter 'password))
    (concatenate 'string "Error, passwords doesn't match.")))

(defmacro validate-field (fieldname errors &rest validators)
  `(let ((lname ,fieldname)
	 (lerrors ,errors))
     (loop for x in (list ,@validators)
	   until (gethash lname lerrors)
	   do (let ((validation-error (funcall x (string-downcase (string lname)))))
		(unless (= (length validation-error) 0)
		  (setf (gethash lname lerrors) validation-error))))))

(defmacro def-validator (name () &body body)
  `(defun ,name ()
     (let ((errors (make-hash-table)))
       (if (eq (hunchentoot:request-method*) :post)
	   (progn
	     ,@body))
       errors)))

(def-validator validate-registration ()
  (validate-field 'fullname errors #'validate-length)
  (validate-field 'username errors #'validate-length
		  #'validate-username #'validate-username-exists)
  (validate-field 'email errors #'validate-length #'validate-email)
  (validate-field 'password errors #'validate-length)
  (validate-field 'cpassword errors #'validate-password))

(def-validator validate-login ()
  (validate-field 'login errors #'validate-length)
  (validate-field 'password errors #'validate-length))

(def-validator validate-newrepository ()
  (validate-field 'name errors #'validate-length))

(def-validator validate-newemail ()
  (validate-field 'email errors #'validate-length
		  #'validate-email #'validate-email-exists))

;;; Path

(defun repository-path (repository)
  (merge-pathnames (slot-value repository 'path)
		   *repository-directory*))

(defun remove-ref-path (ref &optional (substring "refs/heads/"))
  "remove a substring from the start of a string"
  (let ((location (search substring ref)))
    (string-trim " "
		 (if location
		     (subseq ref (length substring))
		     ref))))

;;; View


(defun home-page ()
  (render-standard-page (:title "Planet Git" :subtitle "a bad clone of github")
    (if (loginp) (cl-who:htm (:a :href "/repository/new" "new repository")))))


(def-who-macro repository-item-fragment (name owner public)
  `(cl-who:htm
    (:div :class "well project"
	  (if ,public
	      (cl-who:htm (:span :class "pubilc" "Public"))
	      (cl-who:htm (:span :class "private" "Private")))
	  (:a :href (cl-who:str (url-join ,owner ,name))
	      (:h3 :class "name"
		   (cl-who:str ,name))))))


(define-rest-handler (user-page :uri "^/(\\w+)/?$" :args (username)) ()
  (let
      ((user (car (postmodern:select-dao 'login (:= 'username username)))))
    (if user
	(let ((username (slot-value user 'username))
	      (is-current-user (equal (slot-value user 'username)
				      (when (loginp) (slot-value (loginp) 'username)))))
	  (render-user-page
          (user
           :extra-header (when is-current-user
                           (cl-who:htm (:a :class "btn primary pull-right"
                                           :href "/repository/new"
                                           "Add Repository")))
           :body-class "span11")
	    (let ((repositories (postmodern:select-dao
				 'repository (:= 'owner-id (slot-value user 'id)))))
	      (hunchentoot:log-message* hunchentoot:*lisp-warnings-log-level* "Repositories ~a" repositories)
	      (labels ((repository-fragment (repos)
			 (let* ((repo (car repos)) (rest (cdr repos))
				(visible (or (slot-value repo 'public)
					     (equal (slot-value user 'username)
						    (when (loginp) (slot-value (loginp) 'username)))))
				(public (slot-value repo 'public)))
			   (hunchentoot:log-message* hunchentoot:*lisp-warnings-log-level* "Repository ~a" repo)
			   (when (and repo (or visible is-current-user))
			     (repository-item-fragment (slot-value repo 'name)
						       username
						       public))
			   (when rest (repository-fragment rest)))))
		(when repositories (repository-fragment repositories))))))
	(setf (hunchentoot:return-code*) hunchentoot:+http-not-found+))))


(def-who-macro* email-item-fragment (user email)
  "this fragment renders a users email address as a list item with a
delete button"
  (cl-who:htm
   (:div :class "alert-message"
	 (:a :class "close" :href (cl-who:str
				   (url-join (slot-value user 'username)
					     "settings"
					     "email"
					     (write-to-string (slot-value email 'id))
					     "delete"))
	     (cl-who:str "x"))
	 (cl-who:str (slot-value email 'email)))))


(def-who-macro* user-settings-page (user emails)
		(render-standard-page (:title (cl-who:str (slot-value user 'username))
				       :page-header
				       ((:img :src (gravatar-url
                                    (slot-value user 'email)
                                    :size 40))
                        (:h1 (:a :href (url-join (slot-value user 'username))
                                 (cl-who:str (slot-value user 'username)))
					     (:small "Settings"))))
		  (form-fragment login-form
				 (('fullname "Fullname:" "text" :value (slot-value user 'fullname))
				  ('email "Email:" "text" :value (slot-value user 'email)))
				 :buttons ((:input :type "submit"
						   :class "btn primary"
						   :name "login-form-submit"
						   :value "Save")))
		  (labels ((email-fragment (emails)
			     (let* ((email (car emails)) (rest (cdr emails)))
			       (email-item-fragment user email)
			       (when rest (email-fragment rest)))))
		    (when emails (email-fragment emails)))
		  (form-fragment email-form
				 (('email "Email:" "text"))
				 :buttons ((:input :type "submit"
						   :class "btn primary"
						   :name "email-form-submit"
						   :value "Add")))))


(define-form-handler (user-settings-view :uri "^/(\\w+)/settings/?$" :args (username))
    ((email-form
      (email :parameter-type 'string :request-type :post :validate (#'validate-length #'validate-email)))
     (login-form
      (fullname :parameter-type 'string :request-type :post :validate (#'validate-length))
      (email :parameter-type 'string :request-type :post :validate (#'validate-length #'validate-email))))
  (let*
      ((user (car (postmodern:select-dao 'login (:= 'username username))))
       (is-current-user (when user
                          (equal
                           (slot-value user 'username)
                           (when (loginp)
                             (slot-value (loginp) 'username))))))
    (if is-current-user
        (progn
          (setf *current-form*
                (cond-forms
                 (email-form
                  (postmodern:insert-dao
                   (make-instance 'email
                                  :user-id (slot-value (loginp) 'id)
                                  :email email)))
                 (login-form
                  (let ((user (postmodern:get-dao 'login (slot-value user 'id))))
                    (setf (slot-value user 'fullname) fullname)
                    (setf (slot-value user 'email) email)
                    (postmodern:update-dao user)))))
          (let ((emails (postmodern:select-dao 'email (:= 'user-id (slot-value user 'id)))))
            (user-settings-page user emails)))
        (setf (hunchentoot:return-code*) hunchentoot:+http-forbidden+))))


(define-rest-handler (user-email-delete :uri "^/(\\w+)/settings/email/(\\w+)/delete/?$" :args (username email-id)) ()
  (let*
      ((user (car (postmodern:select-dao 'login (:= 'username username))))
       (is-current-user (when user
			  (equal
			   (slot-value user 'username)
			   (when (loginp)
			     (slot-value (loginp) 'username))))))
    (if is-current-user
	(let ((email (car
		      (postmodern:select-dao 'email
					     (:and (:= 'id email-id)
						   (:= 'user-id (slot-value user 'id)))))))
	  (if email
	      (postmodern:delete-dao email)
	      (setf (hunchentoot:return-code*) hunchentoot:+http-not-found+))
	  (hunchentoot:redirect (url-join (slot-value user 'username) "settings")))
	(setf (hunchentoot:return-code*) hunchentoot:+http-forbidden+))))

(defun add-ssh-key ()
  (let*
      ((req (hunchentoot:request-uri*))
       (username (cl-ppcre:register-groups-bind (username)
		     ("^/(\\w+)/settings/add-key?$" req)
		   username))
       (user (car (postmodern:select-dao 'login (:= 'username username))))
       (is-current-user (when user (equal (slot-value user 'username)
					  (when (loginp) (slot-value (loginp) 'username))))))
					;    (if is-current-user)
    ))


(defun gravatar-url (email &key (size 80))
    (concatenate 'string
		 "http://www.gravatar.com/avatar/"
		 (format nil "~(~{~2,'0X~}~)"
			 (map 'list #'identity (md5:md5sum-sequence (coerce email 'simple-string))))
		 "?s="
		 (prin1-to-string size)))

(defun repository-home-page ()
  (let*
      ((req (hunchentoot:request-uri*))
       (uri-parts (cl-ppcre:register-groups-bind
		      (username repository-name)
		      ("^/([^/]+)/([^/]+)/?$" req)
		    (list username repository-name)))
       (username (car uri-parts))
       (repository-name (car (cdr uri-parts))))
    (repository-page username repository-name)))

(defun repository-branch-page ()
  (let*
      ((req (hunchentoot:request-uri*))
       (uri-parts (cl-ppcre:register-groups-bind
		      (username repository-name branch)
		      ("^/([^/]+)/([^/]+)/branch/([^/]+)/?$" req)
		    (list username repository-name branch)))
       (username (car uri-parts))
       (repository-name (second uri-parts))
       (branch (concatenate 'string "refs/heads/" (third uri-parts))))
    (repository-page username repository-name :branch branch)))

(defun url-join (&rest rest)
  (let ((sequence (mapcan #'(lambda (x) (list (string x) "/")) rest)))
    (reduce #'(lambda (current next)
		(if (stringp next)
		    (concatenate 'string current next)
		    current))
	    sequence
	    :initial-value "/")))

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

(defun repository-page (username repository-name &key branch)
  (let*
      ((user (car (postmodern:select-dao 'login (:= 'username username))))
       (repository (car (postmodern:select-dao
			 'repository (:and
				      (:= 'owner-id (slot-value user 'id))
				      (:= 'name repository-name)))))
       (visible (when repository (or (slot-value repository 'public)
				     (equal (slot-value user 'username)
					    (when (loginp) (slot-value (loginp) 'username))))))
       (is-current-user (when user (equal (slot-value user 'username)
					  (when (loginp) (slot-value (loginp) 'username))))))
    (if (and visible user repository)
	(cl-git:with-git-repository ((repository-path repository))
	  (let* ((branches (cl-git:git-reference-listall))
		 (branch (selected-branch repository branches branch)))
	    (render-standard-page (:title
				   (cl-who:htm (:a :href (url-join (slot-value user 'username))
						   (cl-who:str (slot-value user 'username)))
					       (:span (cl-who:str "/"))
					       (cl-who:str (slot-value repository 'name)))
				   :page-header ((:img :src (gravatar-url
							     (slot-value user 'email)
							     :size 40))))
	      (cond
		(branch
		 (cl-who:htm
		  (:script :type "text/javascript"
			   (cl-who:str
			    (ps:ps
			      (defun select-branch (branch)
				(setf (ps:getprop window 'location 'href)
				      (concatenate 'string
						   (ps:lisp (url-join username repository-name "branch"))
						   branch "/"))))))
		  (:div :class "project-bar"
			(:select :id "branch"
				 :onchange (ps:ps-inline (select-branch
							  (ps:@ this options
								(ps:@ this selected-index) value)))
				 (mapcar #'(lambda (x)
					     (cl-who:htm
					      (:option
					       :value (remove-ref-path x)
					       :selected (when (equal x branch) "true")
					       (cl-who:str (remove-ref-path x)))))
					 (cl-git:git-reference-listall))))
		  (:ol :class "commit-list"
		       (let ((count 0))
			 (cl-git:with-git-revisions (commit :head branch)
			   (setf count (+ count 1))
			   (when (> count 10) (return))
			   (cl-who:htm
			    (:li
			     (let* ((author (cl-git:git-commit-author commit))
				    (name (first author))
				    (email (second author))
				    (timestamp (third author)))
			       (cl-who:htm
				(:img :src (gravatar-url email :size 40))
				(:p
				 (cl-who:str
				  (cl-git:git-commit-message commit)))
				(:span :class "author" (cl-who:str name))
				(:span :class "date"
				       (cl-who:str
					(local-time:format-timestring nil timestamp :format
								      '(:long-month " " :day ", " :year))))))
			     )))))))
		((and (eq branch nil) is-current-user)
		 (cl-who:htm
		  (:div :class "well"
			(:h2 "Welcome to your new repository."))
		  ))
		((eq branch nil)
		 (cl-who:htm
		  (:div :class "well"
			(:h2 "Under Construction."))
		  ))
		(t (setf (hunchentoot:return-code*) hunchentoot:+http-not-found+))))))
	(setf (hunchentoot:return-code*) hunchentoot:+http-not-found+))))


(hunchentoot:define-easy-handler
    (register-page :uri "/register")
    ((fullname :parameter-type 'string :request-type :post)
     (username :parameter-type 'string :request-type :post)
     (password :parameter-type 'string :request-type :post)
     (cpassword :parameter-type 'string :request-type :post)
     (email :parameter-type 'string :request-type :post))
  (let ((errors (validate-registration)))
    (if (and (= (hash-table-count errors) 0)
	     (eq (hunchentoot:request-method*) :post))
	(progn
	  (let ((login (postmodern:insert-dao
			(make-instance 'login
				       :fullname fullname
				       :username username
				       :email email
				       :password password)))
		(session (hunchentoot:start-session)))
	    (postmodern:insert-dao
	     (make-instance 'email
			    :user-id (slot-value login 'id)
			    :email email
			    :rank 0))
	    (setf (hunchentoot:session-value 'user session) login)
	    (hunchentoot:redirect (url-join (slot-value login 'username)))))
	(render-standard-page (:title "Register")
	  (:form :action "" :method "post"
		 (if (> (hash-table-count errors) 0)
		     (cl-who:htm
		      (:div :class "alert-message error"
			    (:p "Error detected on the page"))))
		 (field-fragment "fullname" "Fullname:" "text"
			:value fullname
			:error (gethash 'fullname errors))
		 (field-fragment "username" "Username:" "text"
			:value username
			:error (gethash 'username errors))
		 (field-fragment "email" "Email:" "text"
			:value email
			:error (gethash 'email errors))
		 (field-fragment "password" "Password:" "text"
			:error (gethash 'password errors))
		 (field-fragment "cpassword" "confirm passwd" "text"
			:error (gethash 'cpassword errors))
		 (:div :class "actions"
		       (:input :class "btn primary" :type "submit"
			       :name "register" :value "Register"))))
	  )))


(defun compare-password-hash (passwordhash password)
  (if (string= passwordhash password)
      T
      nil))


(defun verify-password (login password)
  (let* ((user (car (postmodern:query
		     (:select 'login.id 'login.password
			      :from 'login
			      :left-join 'email :on (:= 'login.id 'email.user-id)
			      :where (:or (:= 'login.username login) (:= 'email.email login))))))
	 (user-id (car user))
	 (user-passwd (car (cdr user))))
    (if (compare-password-hash user-passwd password)
	user-id
	nil)))


(defun login-session (login password)
  "log a user out of a session"
  (let ((user-id (verify-password login password)))
    (if user-id
	(let ((session (hunchentoot:start-session))
	      (user (postmodern:get-dao 'login user-id)))
	  (setf (hunchentoot:session-value 'user session) user)
	  )
	nil
	)))


(defun logout-session ()
  "remove the user from the current session-login"
  (hunchentoot:delete-session-value 'user))


(defun loginp ()
  (hunchentoot:session-value 'user))


(hunchentoot:define-easy-handler
    (login-page :uri "/login")
    ((login :parameter-type 'string :request-type :post)
     (password :parameter-type 'string :request-type :post)
     (came-from :parameter-type 'string))
  (let* ((errors (when (and login password) (validate-login)))
	 (logged-in (when (and errors (= (hash-table-count errors) 0)) (login-session login password))))
    (unless (and errors (gethash 'password errors))
      (setf (gethash 'password errors) "Invalid password."))
    (if logged-in
	(hunchentoot:redirect came-from)
	(render-standard-page (:title "Login")
	  (:form :action "" :class "login-form form-stacked" :method "post"
		 (if (> (hash-table-count errors) 0)
		     (cl-who:htm
		      (:div :class "alert-message error"
			    (:p "Error detected on the page"))))
		  (:input :type "hidden" :name "came-from"
			  :value came-from)
		  (field-fragment "login" "Username or Email:" "text"
			       :value login
			       :error (gethash 'login errors))
		  (field-fragment "password" "Password:" "text"
			       :error (gethash 'password errors))
		 (:div :class "actions"
		       (:a :class "btn secondary"
			   :href came-from "Cancel")
		       (:input :class "btn primary"
			       :type "submit"
			       :name "login"
			       :value "Login")))))))


(hunchentoot:define-easy-handler
    (logout-page :uri "/logout") ()
  (logout-session)
  (hunchentoot:redirect "/"))


(defun create-repository (name owner public)
  (let* ((username  (slot-value owner 'username))
	 (relative-path (make-pathname :directory
					       (list ':relative
						     (string username)
						     (string name))))
	 (path (merge-pathnames relative-path
			       *repository-directory*)))
    (ensure-directories-exist path)
    (postmodern:insert-dao
     (make-instance 'repository
		    :owner-id (slot-value owner 'id)
		    :name name
		    :path (namestring relative-path)
		    :public public))
    (cl-git:ensure-git-repository-exist path t)))

(hunchentoot:define-easy-handler
    (new-repository-page :uri "/repository/new")
    ((name :parameter-type 'string)
     (public :parameter-type 'boolean))
  (let* ((errors (validate-newrepository)))
    (if (and (= (hash-table-count errors) 0)
	     (eq (hunchentoot:request-method*) :post))
	(progn
	  (create-repository name (loginp) public)
	  (hunchentoot:redirect (concatenate 'string "/"
					     (slot-value (loginp) 'username) "/"name "/")))
      (render-standard-page (:title "New Repository")
	(:form :action "" :method "post" :class "form-stacked"
	       (if (> (hash-table-count errors) 0)
		   (cl-who:htm
		    (:div :class "alert-message error"
			  (:p "Error detected on the page"))))
	       (field-fragment "name" "Name:" "text"
		      :error (gethash 'name errors))
	       (field-fragment "public" "Public:" "checkbox"
		      :error (gethash 'public errors))
	       (:div :class "actions"
		     (:input :type "submit"
			     :class "btn primary"
			     :name "submit"
			     :value "Create")))))))
