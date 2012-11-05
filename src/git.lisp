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

(defun ensure-git-repository-exist (path &optional bare)
  "Open a repository at location, if the repository doesn't exist
create it.  BARE is an optional argument if specified and true, the newly
created repository will be bare."
  (handler-case
      (progn
        (git-open :repository path)
        path)
    (git-error ()
      ;; TODO should catch error 5 not all errors.
      (git-init :repository path :bare bare)
      path)))

(defun format-git-timestamp (timestamp &optional (format :short))
  "Format a git timestamp in some predefined formats."
  (ecase format
    (:short
     (format-timestring nil timestamp
                        :format '(:long-month " " :day ", " :year)))))
