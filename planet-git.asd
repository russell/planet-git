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

(operate 'load-op :paren-files)

(asdf:defsystem #:planet-git
  :serial t
  :depends-on (:planet-git.widgets
               :anaphora
               :hunchentoot
               :chunga
               :cl-who
               :cl-git
               :postmodern
               :cl-ppcre
               :css-lite
               :cl-json
               :asdf
               :database-migrations
               :parenscript
               :paren-files
               :paren-psos
               :cl-fad
               :split-sequence
               :puri
               :md5
               :swank)
  :components ((:static-file "planet-git.asd")
               (:module "src"
                        :components
                        ((:file "package")
                         (:file "config" :depends-on ("package"))
                         (:file "traverser" :depends-on ("package"))
                         (:file "utils" :depends-on ("package"))
                         (:file "parameters" :depends-on ("package"))
                         (:file "git" :depends-on ("package"))
                         (:file "auth" :depends-on ("package"))
                         (:file "templates" :depends-on ("package" "models" "auth"))
                         (:file "validators" :depends-on ("package"))
                         (:module "models"
                                  :depends-on ("package" "auth")
                                  :components
                                  ((:file "users")
                                   (:file "sql")
                                   (:file "repository")))
                         (:module "paren"
                                  :depends-on ("package")
                                  :components
                                  ((:file "jquery")
                                   (:parenscript-file "commit")))
                         (:module "views"
                                  :depends-on ("package"
                                               "validators"
                                               "templates"
                                               "parameters"
                                               "rest"
                                               "models")
                                  :components
                                  ((:file "home")
                                   (:file "user")
                                   (:file "registration")
                                   (:file "repository")))
                         (:file "css" :depends-on ("package"))
                         (:file "planet-git" :depends-on ("package" "views" "traverser"))))))
