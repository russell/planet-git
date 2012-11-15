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

(defpackage :planet-git.ps
  (:use #:cl #:parenscript #:cl-who #:rjson #:paren-util)
  (:export
   #:ajax
   #:$
   #:doc-ready))

(defpackage :planet-git
  (:use #:cl
        #:hunchentoot
        #:postmodern
        #:planet-git.ps)
  (:import-from #:planet-git.widgets
                #:validate-form
                #:parse-form
                #:form-class
                #:form-widget
                #:render
                #:render-buttons
                #:render-widget
                #:form-real-name
                #:def-validator
                #:wlambda
                #:valid-p
                #:form-errors
                #:widget
                #:form)
  (:import-from #:anaphora
                #:awhen
                #:aif
                #:it)
  (:import-from #:asdf
                #:component-pathname
                #:component-system
                #:find-system)
  (:import-from #:json
                #:*json-output*
                #:encode-json-to-string
                #:encode-json
                #:encode-object-member
                #:stream-object-member-encoder
                #:as-object-member
                #:with-array
                #:with-object
                #:as-array-member
                #:encode-json-alist)
  (:import-from #:chunga
                #:as-keyword)
  (:import-from #:cl-git
                #:git-open
                #:git-init
                #:git-id
                #:git-name
                #:git-author
                #:git-message
                #:git-list
                #:with-repository
                #:with-git-revisions
                #:revision-walk
                #:git-next)
  (:import-from #:cl-fad
                #:directory-exists-p)
  (:import-from #:cl-ppcre
                #:create-scanner
                #:register-groups-bind
                #:scan)
  (:import-from #:cl-who
                #:htm
                #:str
                #:with-html-output
                #:with-html-output-to-string)
  (:import-from #:css-lite
                #:css)
  (:import-from #:parenscript
                #:defpsmacro
                #:ps-compile-file
                #:ps-inline
                #:ps)
  (:import-from #:local-time
                #:format-timestring)
  (:import-from #:md5
                #:md5sum-sequence)
  (:import-from #:parenscript.asdf
                #:compile-script-system)
  (:import-from #:split-sequence
                #:split-sequence)
  (:import-from #:puri
                #:parse-uri
                #:uri-path)
  ;; hunchentoot:shutdown is shadowed by ours
  (:shadow #:shutdown)
  (:export
   #:*git-ssh-host*
   #:*git-user-homedir*
   #:*git-ssh-host*
   #:create-tables
   #:startup
   #:shutdown
   #:main))
