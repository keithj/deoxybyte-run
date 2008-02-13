;;;
;;; Copyright (C) 2008, Keith James. All rights reserved.
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;

(in-package :cl-io-utilities)

(defparameter *empty-field* "")

(defmacro define-line-parser (parser-name delimiter fields
                              &optional constraints)
  (let ((field-count (length fields))
        (field-names (mapcar #'car fields))
        (field-args (mapcar #'collect-parser-args fields))
        (constraint-names (mapcar #'car constraints))
        (constraint-args (mapcar #'collect-constraint-args
                                 constraints)))
    `(progn
       (defun ,parser-name (line)
         (declare (optimize (speed 3)))
         (declare (type simple-string line))
         (multiple-value-bind (field-starts field-ends)
             (vector-split-indices ,delimiter line)
           (declare (type list field-starts))
           (unless (= ,field-count (length field-starts))
             (error 'malformed-record-error :text
                    (format nil (msg "Invalid line having ~d fields"
                                     "instead of ~d: ~s.")
                            (length field-starts) ,field-count line)))
           (let ((parsed-fields
                  (loop
                     for name in (list ,@(mapcar #'(lambda (n)
                                                     `(quote ,n))
                                                 field-names))
                     for arg-list in (list ,@field-args)
                     for start in field-starts
                     for end in field-ends
                     unless (arg-value :ignore arg-list)
                     collect (cons name
                                   (apply #'parse-field name line
                                          start end arg-list)))))
             (let* ((record-constraints
                     (loop
                        for name in (list ,@(mapcar #'(lambda (n)
                                                        `(quote ,n))
                                                    constraint-names))
                        for form in (list ,@constraint-args)
                        collect (apply #'validate-record
                                       name parsed-fields form)))
                    (failed-constraints
                     (loop
                        for (result . nil) on record-constraints
                        when (null (cdr result))
                        collect (car result))))
               (when failed-constraints
                 (error 'record-validation-error :text
                        (format nil "Constraints ~a failed on line ~s."
                                failed-constraints line))))
             parsed-fields))))))

(defun default-string-parser (field-name str &key (start 0) end
                              null-str)
  "Returns a string subsequence from record STR between START and END,
or NIL if STR is STRING= to NULL-STR between START and END."
  (declare (optimize (speed 3)))
  (declare (type simple-string str))
  (let ((end (or end (length str))))
    (if (and null-str (string= null-str str :start2 start :end2 end))
        nil
      (handler-case
          (subseq str start end)
        (parse-error (condition)
          (error 'malformed-field-error :text
                 (format nil "Invalid ~a field ~a: ~a" field-name
                         (subseq str start end) condition)))))))

(defun default-integer-parser (field-name str &key (start 0) end
                               (null-str *empty-field*))
  "Returns an integer parsed from record STR between START and END, or
NIL if STR is STRING= to NULL-STR between START and END."
  (declare (optimize (speed 3)))
  (declare (type simple-string str))
  (let ((end (or end (length str))))
    (if (and null-str (string= null-str str :start2 start :end2 end))
        nil
      (handler-case
          (parse-integer str :start start :end end)
        (parse-error (condition)
          (error 'malformed-field-error :text
                 (format nil "Invalid ~a field ~a: ~a" field-name
                         (subseq str start end) condition)))))))

(defun default-float-parser (field-name str &key (start 0) end
                             (null-str *empty-field*))
  "Returns a float parsed from record STR between START and END, or
NIL if STR is STRING= to NULL-STR between START and END."
  ;; (declare (optimize (speed 3)))
  ;; (declare (type simple-string str))
  (let ((end (or end (length str))))
    (if (and null-str (string= null-str str :start2 start :end2 end))
        nil
      (handler-case
          (parse-float str :start start :end end)
        (parse-error (condition)
          (error 'malformed-field-error :text
                 (format nil "Invalid ~a field ~a: ~a" field-name
                         (subseq str start end) condition)))))))
  
(defun parse-field (field-name line start end null-str parser
                    &optional validator)
  "Returns a value parsed from LINE between START and END using PARSER
and VALIDATOR."
  (declare (optimize (speed 3)))
  (declare (type simple-string line))
  (let ((parsed-value (funcall parser field-name line
                               :start start :end end
                               :null-str null-str)))
    (if validator
        (funcall validator parsed-value)
      parsed-value)))

(defun validate-record (name fields validator &rest field-names)
  "Returns a pair of constraint NAME and either T or NIL, indicating
the result of applying VALIDATOR to values from the alist of parsed
FIELDS named by FIELD-NAMES."
  ;; (declare (optimize (speed 3)))
  ;; (declare (type function validator))
  (let ((field-values (mapcar #'(lambda (key)
                                  (assocdr key fields)) field-names)))
    (cons name (not (null (apply validator field-values))))))

(defun collect-parser-args (field)
  "Returns an argument list form for FIELD to be used by PARSE-FIELD
which has suitable parsers and validators set up for the standard
field types: :string, :integer and :float."
  (destructuring-bind (field-name &key ignore (type :string)
                                  null-str parser validator)
      field
    (declare (ignore field-name))
    (if ignore
        `(list :ignore t)
      (let ((field-parser
             (or parser (ecase type
                          (:string #'default-string-parser)
                          (:integer #'default-integer-parser)
                          (:float #'default-float-parser))))
            (field-validator
             (or validator (ecase type
                             (:string nil)
                             (:integer nil)
                             (:float nil)))))
        `(list ,null-str ,field-parser ,field-validator)))))

(defun collect-constraint-args (form)
  "Returns an argument list form to be used by CROSS-VALIDATE by
quoting the field-names in FORM and re-ordering the elements."
  (destructuring-bind (constraint-name field-names validator)
      form
    (declare (ignore constraint-name))
    `(list ,validator ,@(mapcar #'(lambda (n)
                                    `(quote ,n))
                                field-names))))
