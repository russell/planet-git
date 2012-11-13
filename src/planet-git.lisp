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

(require :swank)
(require :hunchentoot)
(require :cl-who)
(require :postmodern)
(require :cl-ppcre)

;;; Webserver

(defun resource-path (path)
  "looks up path relative to whereever this asdf system is installed.  Returns a truename"
  (truename (asdf:system-relative-pathname :planet-git path)))

(setq *rest-handler-alist*
      (list
       (list "^/(\\w+)/settings/?$" t "text/html" 'user-settings-page)
       (list "^/(\\w+)/settings/email/(\\w+)/delete/?$" t "text/html" 'user-email-delete)
       (list "^/(\\w+)/settings/key/(\\w+)/delete/?$" t "text/html" 'user-key-delete)
       (list "^/(\\w+)/settings/add-key?$" t "text/html" 'add-ssh-key)
       (list "^/[^/]+/[^/]+/$" t "text/html" 'repository-home-page)
       (list "^/[^/]+/[^/]+/key/[^/]+/$" t "text/html" 'repository-key-access)
       (list "^/[^/]+/[^/]+/branch/[^/]+/$" t "text/html" 'repository-branch-page)
       (list "^/[^/]+/[^/]+/commits/[^/]+/$" t "application/json" 'repository-branch-commits-json)))

(setq *dispatch-table*
 (list
  #'dispatch-traverser-handlers
  #'dispatch-easy-handlers
  #'dispatch-rest-handlers
  (create-prefix-dispatcher "/static/base.css" #'base-css)
  (create-folder-dispatcher-and-handler "/static/" (resource-path "static"))))


;;; Database
(defun create-tables ()
  (unless (table-exists-p 'login)
    (execute (dao-table-definition 'login)))
  (unless (table-exists-p 'email)
    (execute (dao-table-definition 'email)))
  (unless (table-exists-p 'key)
    (execute (dao-table-definition 'key)))
  (unless (table-exists-p 'repository)
    (execute (dao-table-definition 'repository))))
