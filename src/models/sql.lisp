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

(defparameter *max-results-per-query* 5
  "The max number of results to return per query.")


(defmacro count-dao (type &optional (test t))
  "Select count the daos for which the given test holds."
  (flet ((check-string (x)
           (if (stringp x) `(:raw ,x) x)))
    (let* ((type-name (gensym))
           (count (gensym))
           (query `(:select (:count '*) :from (dao-table-name (find-class ,type-name))
                    :where ,(check-string test))))
      `(let ((,type-name ,type)
             ,count)
         (doquery ,query (count)
           (setq ,count count))
         ,count))))
