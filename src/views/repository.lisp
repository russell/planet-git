;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10 -*-

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

;; the current url format is
;; /username/repository
;;

(defgeneric repository-page (method content-type &key username repository &allow-other-keys))

(defmethod repository-page ((method (eql :get)) (content-type (eql :html))
                            &key username repository)
  (repository-page-widget username repository))

(defmethod repository-page ((method (eql :get)) (content-type (eql :html))
                            &key username repository branch)
  (let ((ref (concatenate 'string "refs/heads/" branch)))
    (repository-page-widget username repository :branch ref)))



(define-rest-handler (repository-home-page
                      :uri "^/([^/]+)/([^/]+)/?$"
                      :args (username repository-name))
    ()
    (repository-page-widget username repository-name))


(define-rest-handler (repository-branch-page
                      :uri "^/([^/]+)/([^/]+)/branch/([^/]+)/?$"
                      :args (username repository-name branch))
    ()
  (let
      ((ref (concatenate 'string "refs/heads/" branch)))
    (repository-page-widget username repository-name :branch ref)))


(define-rest-handler (repository-branch-commits-json
                      :uri "^/([^/]+)/([^/]+)/commits/([^/]+)/?$"
                      :args (username repository-name branch)
                      :content-type "application/json")
    ((ref :parameter-type 'string :request-type :both))
  (let* ((head-ref (concatenate 'string "refs/heads/" branch))
         (user (car (select-dao 'login (:= 'username username))))
         (repository (car (select-dao
                              'repository (:and
                                           (:= 'owner-id (slot-value user 'id))
                                           (:= 'name repository-name))))))

    (setf (content-type*) "application/json")
    (with-html-output-to-string (*standard-output*)
	(with-repository ((repository-real-path repository))
      (let* ((branches (git-list :reference))
             (branch (selected-branch repository branches head-ref))
             (walker (revision-walk (or ref branch))))
        (with-array ()
          (dotimes (count 10 t)
            (let ((commit (git-next walker)))
              (when (not commit) (return))
              (let* ((author (git-author commit))
                     (name (getf author :name))
                     (email (getf author :email))
                     (timestamp (getf author :time)))
                (as-array-member ()
                    (encode-json-alist
                     (eval `(quote
                             (("id" . ,(git-name commit))
                              ("icon" . ,(gravatar-url email :size 40))
                              ("message" . ,(git-message commit))
                              ("name" . ,name)
                              ("time" . ,(format-git-timestamp timestamp :short))))))))))))))))


(define-rest-handler (repository-key-access
                      :uri "^/([^/]+)/([^/]+)/key/([^/]+)/?$"
                      :args (username repository-name key-id))
    ()
  (let ((key-id (parse-integer key-id)))
    (if
     (let ((owner-key (query (:select '*
                       :from 'key
                       :inner-join 'login :on (:= 'key.user-id 'login.id)
                       :inner-join 'repository :on (:= 'login.id 'repository.owner-id)
                       :where (:and (:= 'key.id key-id)
                                    (:= 'repository.name repository-name)
                                    (:= 'login.username username))))))
       owner-key)

     (setf (return-code*) +http-no-content+)
     (setf (return-code*) +http-forbidden+)))
  "") ;; return an empty string for the content

(defun string-concat (&rest strings)
  (apply #'concatenate 'string strings))

(def-validator sensible-repository-name
  (when (car (list (scan "[^a-zA-Z0-9.\-_]" value)))
    (string-concat "Error, " fieldname " can only contain alphanumeric, -, _ or . characters.")))

(defclass repository-form (form)
  ((name :parameter-type string
         :request-type :post
         :initform ""
         :type :text
         :validators (:required :sensible-repository-name))
   (public :parameter-type string
           :request-type :post
           :initform nil
           :type :checkbox))
  (:metaclass form-class))

(defmethod save-form ((form repository-form))
  "save form then reset it's content."
  (create-repository (slot-value form 'name) (loginp) (slot-value form 'public)))

(defgeneric new-repository-page (method content-type &key username))

(defmethod new-repository-page ((method (eql :get)) (content-type (eql :html)) &key username)
  (let ((repo-form (make-instance 'repository-form :submit-action "Create"))
        (is-current-user (is-current-user-p username)))
    (if is-current-user
        (render-standard-page (:title "New Repository")
          (render-widget repo-form))
        (setf (return-code*) +http-forbidden+))))

(defmethod new-repository-page ((method (eql :post)) (content-type (eql :html)) &key username)
  (let ((repo-form (make-instance 'repository-form :submit-action "Create"))
        (is-current-user (is-current-user-p username)))
    (if is-current-user
        (if (validate-form (parse-form repo-form))
            (redirect (url-for (save-form repo-form) :get))
            (render-standard-page (:title "New Repository")
              (render-widget repo-form)))
        (setf (return-code*) +http-forbidden+))))

(define-easy-handler (repository-js :uri "/static/repository.js")
    ()
    (setf (content-type*) "text/javascript")
    (ps-compile-file
     (merge-pathnames
      (make-pathname :directory (list :relative "src" "paren")
                     :name "commit" :type "paren")
      (component-pathname (component-system (find-system :planet-git))))))

(define-easy-handler (psos-js :uri "/static/psos.js")
    ()
    (setf (content-type*) "text/javascript")
  (with-output-to-string (js-output)
    (compile-script-system :paren-psos :output-stream js-output :pretty-print nil)))

(def-who-macro* widget-repository-commits (username repository-name branch)
  (htm
   (:script :type "text/javascript"
            (str
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
                              (htm
                               (:option
                                :value (remove-ref-path x)
                                :selected (when (equal x branch) "true")
                                (str (remove-ref-path x)))))
                          (git-list :reference))))
   (widget-tabs
    ("Commits"
     (:ol :id "commit-list" :class "commit-list"
          :user username :repository repository-name
          :branch (remove-ref-path branch)
          ;; this should probably be added back once i
          ;; can find a way to get the ajax interface to
          ;; play nice with a non-ajax interface.

          ;; (let ((count 0))
          ;;   (with-git-revisions (commit :head branch)
          ;;     (setf count (+ count 1))
          ;;     (when (> count 10) (return))
          ;;     (htm
          ;;      (:li :id (git-id commit)
          ;;           (let* ((author (git-author commit))
          ;;                  (name (getf author :name))
          ;;                  (email (getf author :email))
          ;;                  (timestamp (getf author :time)))
          ;;             (htm
          ;;              (:img :src (gravatar-url email :size 40))
          ;;              (:p (str (git-message commit)))
          ;;              (:span :class "author" (str name))
          ;;              (:span :class "date"
          ;;                     (str
          ;;                      (format-timestring nil timestamp :format
          ;;                                         '(:long-month " " :day ", " :year))))))))))
          ))
    ("Files"
     (:table :class "table")))))

(defun repository-page-widget (username repository-name &key branch)
  (let*
      ((user (car (select-dao 'login (:= 'username username))))
       (repository (car (select-dao
                            'repository (:and
                                         (:= 'owner-id (slot-value user 'id))
                                         (:= 'name repository-name)))))
       (visible (when repository (or (slot-value repository 'public)
                                     (equal (slot-value user 'username)
                                            (when (loginp) (slot-value (loginp) 'username))))))
       (is-current-user (when user (equal (slot-value user 'username)
                                          (when (loginp) (slot-value (loginp) 'username))))))
    (if (and visible user repository)
        (with-repository ((repository-real-path repository))
          (let* ((branches (git-list :reference))
                 (branch (selected-branch repository branches branch)))
            (render-user-page (user :title
                                    (htm (:a :href (url-join (slot-value user 'username))
                                             (str (user-username user)))
                                         (:span (str "/"))
                                         (str (repository-name repository)))
                                    :subtitle ""
                                    :extra-head ((:script :type "text/javascript" :src "/static/psos.js")
                                                 (:script :type "text/javascript" :src "/static/repository.js")))
              (cond
                (branch
                 (widget-repository-commits username repository-name branch))
                ((and (eq branch nil) is-current-user)
                 (htm
                  (:div :class "well"
                        (:h2 "Welcome to your new repository.")
                        (:p "First things first, if you haven't already done so,
            you should set up your user preferences."
                            (:pre (format t "git config --global user.name \"~A\"
git config --global user.email \"~A\"" (user-fullname user) (user-primary-email user))))
                        (:p "If you are adding an existing repository begin by,"
                            (:pre (format t "cd existing_repository
git remote add origin ~A:~A/~A
git push origin master" *git-ssh-host* (user-username user) (repository-name repository)))))))
                ((eq branch nil)
                 (htm
                  (:div :class "well"
                        (:h2 "Under Construction."))))
                (t (setf (return-code*) +http-not-found+))))))
        (setf (return-code*) +http-not-found+))))
