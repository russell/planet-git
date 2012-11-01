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


(in-package #:planet-git-test)

(in-suite :planet-git)

(defparameter test-traversal-path
  '(nil 'home-page
    ("register" 'register-page)
    (:user 'user-page
     ("settings" 'user-settings-page
      ("email" 'user-email-page)
      ("key" 'user-key-page))
     (:repository 'repository-home-page
      ("key" 'repository-key-access)
      ("branch" 'repository-branch-page
       (:branch 'repository-branch-page))
      ("commits" 'repository-commits)))))

(test traverse-path
  (is (equal '('repository-home-page (:repository "repository" :user "russell"))
             (planet-git::traverse-path '("" "russell" "repository") test-traversal-path))))

(test traverse-path1
  (is (equal '('user-email-page (:user "russell"))
             (planet-git::traverse-path '("" "russell" "settings" "email") test-traversal-path))))

(test traverse-path2
  (is (equal '('repository-branch-page (:branch "master" :repository "repository" :user "russell"))
             (planet-git::traverse-path '("" "russell" "repository" "branch" "master") test-traversal-path))))

(test traverse-path3
  (is (equal '('repository-branch-page (:repository "repository" :user "russell"))
             (planet-git::traverse-path '("" "russell" "repository" "branch") test-traversal-path))))
