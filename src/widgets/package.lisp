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

(in-package :cl-user)

(defpackage :planet-git.widgets
  (:use #:closer-common-lisp #:closer-mop)
  (:import-from #:chunga
                #:as-keyword)
  (:import-from #:cl-ppcre
                #:register-groups-bind
                #:scan)
  (:import-from #:cl-who
                #:str
                #:with-html-output
                #:htm)
  (:import-from #:hunchentoot
                #:parameter-error
                #:get-parameters*
                #:post-parameters*
                #:get-parameter
                #:post-parameter
                #:parameter
                #:header-in
                #:*request*)
  (:import-from #:anaphora
                #:awhen
                #:it)
  (:export
   #:validate-form
   #:parse-form
   #:form-class
   #:render
   #:render-buttons
   #:form-real-name
   #:def-validator
   #:valid-p
   #:form-errors
   #:wlambda
   #:widget
   #:form
   #:form-widget))
