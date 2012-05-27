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


(defmacro def-who-macro (name (&rest args) &optional documentation pseudo-html-form)
  "A macro for use with CL-WHO's WITH-HTML-OUTPUT."
  (let ((documentation (if (stringp documentation) documentation ""))
	(pseudo-html-form (if (stringp documentation) pseudo-html-form documentation)))
    `(defmacro ,name (,@args)
       ,documentation
       `(with-html-output (*standard-output* nil)
	  ,,pseudo-html-form))))

(defmacro def-who-macro* (name (&rest args) &optional documentation pseudo-html-form)
  "Who-macro, which evaluates its arguments (like an ordinary function,
which it is in fact.  Useful for defining syntactic constructs"
  (let ((documentation (if (stringp documentation) documentation ""))
	(pseudo-html-form (if (stringp documentation) pseudo-html-form documentation)))
    `(defun ,name (,@args)
       ,documentation
       (with-html-output (*standard-output* nil)
	 ,pseudo-html-form))))

(defmacro render-standard-page ((&key title (subtitle "") (body-class "span10") page-header) &body body)
  `(with-html-output-to-string (*standard-output* nil :prologue t)
     (:html :xmlns "http://www.w3.org/1999/xhtml"
	    :xml\:lang "en"
	    :lang "en"
	    (:head
	     (:meta :http-equiv "Content-Type"
                :content "text/html;charset=utf-8")
	     (:title "Planet Git - " ,title)
	     (:link :rel "stylesheet" :href "/static/css/bootstrap.css")
	     (:link :rel "stylesheet" :href "/static/base.css")
	     (:script :type "text/javascript" :src "/static/js/jquery.js")
	     (:script :type "text/javascript" :src "/static/js/bootstrap.js"))
	    (:body
	     (:div :class "navbar"
		   (:div :class "navbar-inner"
			 (:div :class "container"
			       (:a :class "brand" :href "/" "Planet Git")
			       (:ul :class "nav")
			       (:ul :class "nav pull-right"
				    (if (loginp)
					(let ((username (slot-value (loginp) 'username)))
					  (htm
					   (:li (:a :href (url-join username) (str username)))
					   (:li (:a :href (url-join username "settings") (str "Settings")))
					   (:li (:a :href "/logout" "Logout")))))
				    (unless (loginp)
				      (htm
				       (modal ("login-modal"
                               "Login"
                               :buttons ((:a :href "#" :class "btn btn-primary"
                                             :onclick (ps:ps-inline
                                                       ($ "#login-modal-form"
                                                                  (submit)))

                                             "Login")
                                         (:a :href "#" :class "btn"
                                             :onclick (ps:ps-inline
                                                       ($ "#login-modal"
                                                                  (modal "hide")))
                                             "Cancel")))
                         (:form :id "login-modal-form" :class "login-form"
                                :action "/login" :method "post"
                                (:ul
                                 (:input :type "hidden" :name "came-from"
                                         :value (request-uri*))
                                 (:li "Username or Email:")
                                 (:li (:input :type "text" :name "login"))
                                 (:li "Password:")
                                 (:li (:input :type "password" :name "password")))
                                (:input :type "submit"
                                        :style "visibility: hidden;"
                                        :name "create"
                                        :value "Create"))
                         (:script :type "text/javascript"
                                  (str
                                   (ps:ps
                                     (doc-ready
                                      ($ "#login-modal"
                                         (on "shown"
                                             (lambda ()
                                               (console.log "hello")
                                               ($ "#login-modal-form input[name=\"login\"]"
                                                  (focus))))))))))
				       (:li (:a :href "/register" "Register"))
				       (:li (:a :href "/login"
                                :data-target "#login-modal"
                                :data-toggle "modal"
                                "Login"))))))))
	     (:div :class "container"
		   (:div :class "content"
			 (:div :class "page-header"
			       ,(if page-header
                        `(htm ,page-header)
                        `(htm (:h1 ,title
                                          (:small ,subtitle)))))
			 (:div :class "row"
			       (:div :class ,body-class
			       ,@body))))))))


(defmacro render-user-page ((user &key title subtitle (body-class "span10") extra-header) &body body)
  `(render-standard-page
       (:body-class ,body-class
        :title (str (slot-value ,user 'username))
        :page-header
        ((:img :src (user-gravatar-url ,user :size 40))
         (:h1 ,(or title `(:a :href (url-join (slot-value ,user 'username))
                           (str (slot-value ,user 'username))))
         (:small ,(or subtitle `(str (slot-value ,user 'fullname)))))
         ,(when extra-header extra-header)))
     ,@body))


(def-who-macro modal ((id heading &key buttons) &body body)
  (let ((buttons (if buttons buttons
		     '((:a :href "#" :class "btn btn-primary" "Primary")
		       (:a :href "#" :class "btn" "Secondary")))))
    `(:div :id ,id :class "modal hide fade"
		 (:div :class "modal-header"
               (:button :class "close" :data-dismiss "modal" "&times;")
		       (:h3 ,heading))
		 (:div :class "modal-body"
		       ,@body)
		 (:div :class "modal-footer"
		       ,@buttons))))


(def-who-macro tabs (&rest tabs)
  "Generate a bootstrap tab system, the first element of the tabs is
the NAME of the tab, it will be lowercased and used as the ID of each
tab too.  The other elements of a tab are treated as the body of the
tab."
  ;; TODO if the tab name contains a space convert it to a -
  (let ((default-tab (caar tabs)))
    `(htm
      (:ul :class "nav nav-tabs"
           ,@(mapcar
              (lambda (tab)
                (let ((tab-name (car tab)))
                  `(:li
                    :class ,(if (equal tab-name default-tab) "active" "")
                    (:a :href ,(concatenate 'string "#" (string-downcase tab-name))
                        :data-toggle "tab"
                        (str ,tab-name)))))
              tabs))
      (:div :class "tab-content"
            ,@(mapcar
               (lambda (tab)
                 (let ((tab-name (car tab)))
                   `(:div :id ,(string-downcase tab-name)
                          :class ,(if (equal tab-name default-tab) "tab-pane active" "tab-pane")
                          ,@(cdr tab))))
               tabs)))))
