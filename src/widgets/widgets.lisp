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

(in-package #:planet-git.widgets)

(defclass widget ()
  ((context :initarg :context
            :accessor widget-context
            :documentation "variables that are substitute into the page.")
   (view :initarg :view
            :accessor view
            :documentation "page contents.")))

(defgeneric render-widget (widget))

(defmethod render-widget ((widget widget))
  (funcall (view widget) widget))

(defmacro make-widget (&body pseudo-html-form)
  (let ((widget (gensym)))
    `(prog1
         (let ((,widget (make-instance 'widget))
               (func (lambda (widget)
                       (declare (ignore widget))
                       (with-html-output (*standard-output* nil)
                         ,@pseudo-html-form))))
           (setf (slot-value ,widget 'view) func))
           ,widget)))

(defclass form-class (standard-class)
  ())

(defmethod validate-superclass ((class form-class) (super-class standard-class))
  t)
(defmethod validate-superclass ((super-class standard-class) (class form-class))
  t)

(defclass form ()
  ((errors :initarg :errors
           :initform (make-hash-table)
           :accessor form-errors
           :documentation "the errors associated with this form.")
   (action :initarg :action
           :initform ""
           :documentation "the form submit action.")
   (submit-action :initarg :submit-action
                  :initform "Save"
                  :documentation "the text on the default sumbit button.")
   (method :initarg :method
           :initform :post
           :documentation "the form submit method.")
   (real-name :initarg :real-name
              :accessor form-real-name
              :documentation "the real name of the form."))
  (:metaclass form-class))

(defmethod shared-initialize :after ((class form) slot-names &key &allow-other-keys)
  (declare (ignore slot-names))
  ;; Set a default value for real-name if there isn't one specified
  (when (and class (not (slot-boundp class 'real-name)))
    (setf (slot-value class 'real-name) (compute-real-form-name (class-name (class-of class))))))

(defclass direct-field-slot (standard-direct-slot-definition)
  ((parameter-type :initarg :parameter-type :reader field-type)
   (request-type :initarg :request-type :reader field-request-type)
   (validators :initarg :validators :reader field-validators)
   (type :initarg :type :reader field-type :initform :text)
   (label :initarg :label :initform "")
   (help-text :initarg :widget :initform "")
   (real-name :initarg :real-name))
  (:documentation "Type of slots that refer to database columns."))

(defmethod form-field-slots ((class form))
  "Enumerate the slots in a class that refer to form fields."
  (remove-if-not (lambda (x) (typep x 'direct-field-slot))
                 (class-direct-slots (class-of class))))

(defmethod direct-slot-definition-class ((class form-class) &key parameter-type request-type validate
                                         &allow-other-keys)
  "Slots that have a :parameter-type option are form fields."
  (if (or parameter-type request-type validate)
      (find-class 'direct-field-slot)
      (call-next-method)))

(defmethod compute-effective-slot-definition ((class form-class) name direct-slot-definitions)
  (declare (ignore name))
  (flet ((is-field (slot) (typep slot 'direct-field-slot)))
    (let ((slot (find-if #'is-field direct-slot-definitions)))

      ;; Set a default value for real-name if there isn't one
      ;; specified.
      (when (and slot (not (slot-boundp slot 'real-name)))
        (setf (slot-value slot 'real-name) (compute-real-name name)))

      ;; Set a default value for validators if there isn't one
      ;; specified.
      (when (and slot (not (slot-boundp slot 'validators)))
        (setf (slot-value slot 'validators) ()))

      ;; Set a default value for lable if there isn't one specified.
      (when (and slot (equal "" (slot-value slot 'label)))
        (setf (slot-value slot 'label)
              (format nil "~@(~a~):" (symbol-name name))))

      ;; Set a default request type if there isn't one.
      (when (and slot (not (slot-boundp slot 'request-type)))
        (setf (slot-value slot 'request-type) :both))

      (call-next-method))))

(defmethod slot-direct-definition ((class form) slot-name)
  "return the slot definition for slot named SLOT-NAME."
  (find slot-name (class-direct-slots (class-of class))
        :key #'slot-definition-name))

;;
;; Form views
;;

(defun form-widget (form &key (class "form-horizontal"))
    (with-html-output (*standard-output* nil)
      (htm
       (:form :id (string-downcase (symbol-name 'login-form))
              :action (slot-value form 'action)
              :method (string-downcase (symbol-name (slot-value form 'method)))
              :class class
              (when (not (= (hash-table-count (form-errors form)) 0))
                (htm
                 (:div :class "alert alert-error"
                       (:button :class "close" :data-dismiss "alert" :type "button" "x")
                       (:strong "error:") " found in the form.")))
              (dolist (field (form-field-slots form))
                (render-field form (field-type field) (slot-definition-name field)))
              (render-buttons form)))))

(defmethod render-widget ((form form))
  (form-widget form))

(defgeneric render-buttons (form)
  (:method ((form form))
    (with-html-output (*standard-output* nil)
      (htm
       (:div :class "form-actions"
             (:button :class "btn btn-primary" :type "submit" :name (form-real-name form) :value
                      "login" (str (slot-value form 'submit-action))))))))

;;
;; Input field views
;;

(defun render-input (form field-type field-name)
  (with-html-output (*standard-output* nil)
    (:div
     :class (if (field-error form field-name) "control-group error" "control-group")
     (:label :class "control-label" (str (field-label form field-name)))
     (:div :class "controls"
           (:input :type (string-downcase (symbol-name field-type))
                   :name (field-name form field-name)
                   :class (when (field-error form field-name) "error")
                   :value (slot-value form field-name)
                   :error (field-error form field-name))
           (:span :class "help-inline"
                  (str (field-error form field-name)))))))


(defgeneric render-field (form field-type field-name))

(defmethod render-field ((form form) (field-type (eql :text)) field-name)
  (render-input form field-type field-name))

(defmethod render-field ((form form) (field-type (eql :text-area)) field-name)
  (with-html-output (*standard-output* nil)
    (:div
     :class (if (field-error form field-name) "control-group error" "control-group")
     (:label :class "control-label" (str (field-label form field-name)))
     (:div :class "controls"
           (:textarea :name (field-name form field-name)
                      :rows 3
                      :class (if (field-error form field-name) "input-xlarge" "input-xlarge error")
                      :value (slot-value form field-name)
                      :error (field-error form field-name))
           (:span :class "help-inline"
                  (str (field-error form field-name)))))))

(defmethod render-field ((form form) (field-type (eql :password)) field-name)
  (render-input form field-type field-name))

(defmethod render-field ((form form) (field-type (eql :hidden)) field-name)
  (with-html-output (*standard-output* nil)
    (:input :type (string-downcase (symbol-name field-type))
            :name (field-name form field-name)
            :value (slot-value form field-name))))

(defmethod render-field ((form form) (field-type (eql :email)) field-name)
  (render-input form field-type field-name))

(defmethod render-field ((form form) (field-type (eql :tel)) field-name)
  (render-input form field-type field-name))

(defmethod render-field ((form form) (field-type (eql :number)) field-name)
  (render-input form field-type field-name))

(defmethod parse-form ((form form))
  (let ((default-parameter-type 'string)
        (default-request-type :both))
    (dolist (slot (form-field-slots form))
      (let ((name (slot-definition-name slot)))
        (with-slots (parameter-type request-type real-name)
            slot
          (awhen (compute-parameter real-name
                                    (or parameter-type default-parameter-type)
                                    (or request-type default-request-type))
            (setf (slot-value form name) it))))))
  form)

(defmethod validate-form ((form form))
  "Validate all the fields associated with the form."
  (if (post-parameter (form-real-name form))
      (progn
        (dolist (slot (form-field-slots form))
          (let* ((name (slot-definition-name slot)))
            (validate-field form name)))
        (valid-p form))
      nil))

(defmethod validate-field ((form form) slot-name)
  "Validate a field from the form FORM, update the error dictionary"
  (let ((slot (slot-direct-definition form slot-name)))
    (awhen (validate slot-name
                     (slot-value form slot-name)
                     (slot-value slot 'validators))
      (setf (gethash slot-name (form-errors form)) it))))

(defmethod valid-p ((form form))
  (= (hash-table-count (form-errors form)) 0))

(defmethod field-error ((form form) slot-name)
  (gethash slot-name (slot-value form 'errors)))

(defmethod field-name ((form form) slot-name)
  (let ((slot (slot-direct-definition form slot-name)))
    (slot-value slot 'real-name)))

(defmethod field-value ((form form) slot-name)
  (let ((slot (slot-direct-definition form slot-name)))
    (slot-value slot 'real-name)))

(defmethod field-label ((form form) slot-name)
  (let ((slot (slot-direct-definition form slot-name)))
    (slot-value slot 'label)))
