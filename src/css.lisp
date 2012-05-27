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


(defun base-css ()
  (setf (content-type* *reply*) "text/css")
  (css
    (("html, body")
     (:background-color "#eee"))
    ((".container > footer p")
     (:text-align "center")) ; center align it with the container

    ((".container")
     (:width "820px")); downsize our container to make the content feel a bit tighter and more cohesive. NOTE: this removes two full columns from the grid, meaning you only go to 14 columns and not 16.

                                        ; The white background content wrapper
    ((".content")
     (:background-color "#fff"
      :padding "20px"
      :margin "0 -20px"; negative indent the amount of the padding to maintain the grid system
      :-webkit-border-radius "0 0 6px 6px"
      :-moz-border-radius "0 0 6px 6px"
      :border-radius "0 0 6px 6px"
      :-webkit-box-shadow "0 1px 2px rgba(0,0,0,.15)"
      :-moz-box-shadow "0 1px 2px rgba(0,0,0,.15)"
      :box-shadow "0 1px 2px rgba(0,0,0,.15)"))
                                        ; Page header tweaks
    ((".page-header")
     (:background-color "#f5f5f5"
      :padding "20px 20px 10px"
      :margin "-20px -20px 20px")
     (("h1")
      (:display "inline"
       :vertical-align "top"))
     (("span")
      (:margin ("0 5px")))
     (("small")
      (:margin ("0 5px")))
     (("img")
      (:margin-right "5px")))
    ((".content .span10, .content .span4")
     (:min-height "500px"))

                                        ; Give a quick and non-cross-browser friendly divider
    ((".content .span4")
     (:margin-left "0"
      :padding-left "19px"
      :border-left "1px solid #eee"))

    ((".navbar .btn")
     (:border "0"))

    (("ol.commit-list")
     (:list-style-type "none")
     (("li")
      (:height "40px"
       :margin "10px"
       :padding-left "40px"))
     (("p")
      (:margin "10px"
       :margin "5px"
       :font-weight "bold"))
     ((".author")
      (:margin "5px"))
     ((".date")
      (:font-style "italic"
       :font-size "smaller"))
     (("img")
      (:float "left"
       :margin-left "-40px")))

    ((".project") nil
     (("h3")
      (:display "inline"))
     ((".label")
      (:vertical-align "top")))

    ((".project-bar")
     (:height "27px")
     (("#branch")
      (:float "right")))

    ((".user") nil
     (("h3")
      (:display "inline"
       :vertical-align "top"
       :font-size "32px"))
     (("img")
      (:margin "5px")))

    ((".login-form") nil
     (("ul")
      (:list-style-type "none"))
     (("input")
      (:font-size "large"))
     (("input.btn")
      (:font-size "small")))
    ))
