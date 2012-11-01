;; Planet-Git a source code repository manager.
;; Copyright (C) 2012 Russell Sim <russell.sim@gmail.com>
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

(defparameter *traversal-path*
  '(nil 'home-page
    ("register" 'register-page)
    (:user 'user-page
     ("settings" 'user-settings-page
      ("email" 'user-email-page)
      ("key" 'user-key-page))
     (:repository 'repository-home-page
      ("key" 'repository-key-access)
      ("branch" 'repository-branch-page)
      ("commits" 'repository-commits)))))

(defun traverse-path (path &optional (tree *traversal-path*))
  (let (interesting-parts)
    (labels ((walk-uri (sub-uri sub-tree)
               (destructuring-bind (segment func &rest sub-tree1) sub-tree
                 (let ((uri-segment (car sub-uri))
                       (uri-rest (cdr sub-uri)))
                   (when (or (null segment) (equal segment uri-segment) (symbolp segment))
                     (when (and (symbolp segment) (not (null segment)))
                       (setf interesting-parts `(,segment ,uri-segment ,@interesting-parts)))
                     (aif (cond
                            ((null uri-rest)
                             func)
                            ((not (equal segment uri-segment))
                             nil))
                          it
                          (progn
                            (dolist (branch sub-tree1)
                              (awhen (walk-uri uri-rest branch)
                                (return it))))))))))
      (when (> (length path) 1)
        (cons (walk-uri path tree) (list interesting-parts))))))

;; (traverse-path '("" "russell" "repository"))
;; ('REPOSITORY-HOME-PAGE (:REPOSITORY "repository" :USER "russell"))

(defun dispatch-handlers (request)
  (let ((path (split-sequence #\/ (uri-path (parse-uri (script-name request))))))
    ;; traverse
    ;; resolve and call returned function.
    ))
