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

;;;; package.lisp
(in-package :cl-user)

(defpackage :planet-git
  (:use #:cl
        #:hunchentoot
        #:postmodern)
  (:import-from #:anaphora
                #:awhen
                #:it)
  (:import-from #:asdf
                #:component-pathname
                #:component-system
                #:find-system)
  (:import-from #:chunga
                #:as-keyword)
  (:import-from #:cl-git
                #:ensure-repository-exist
                #:commit-author
                #:commit-message
                #:git-reference-listall
                #:with-repository
                #:with-git-revisions)
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

(defpackage :planet-git.ps
  (:use :parenscript :cl-who)
  (:export #:commit-template))
