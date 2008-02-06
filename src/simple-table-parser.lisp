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
         (multiple-value-bind (field-starts field-ends)
             (vector-split-indices ,delimiter line)
           (unless (= ,field-count (length field-starts))
             (error 'malformed-record-error :text
                    (format nil (msg "Invalid line having ~a fields"
                                     "instead of ~a: ~a.")
                            ,field-count (length field-starts) line)))
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
                                   (apply #'parse-field line
                                          start end arg-list)))))
             (let ((cross-validated
                    (loop
                       for name in (list ,@(mapcar #'(lambda (n)
                                                       `(quote ,n))
                                                   constraint-names))
                       for form in (list ,@constraint-args)
                       collect (apply #'cross-validate
                                      name parsed-fields form))))
               (flet ((failed-validation-p (x)
                        (null (cdr x))))
                 (when (some #'failed-validation-p cross-validated)
                   (error 'malformed-record-error :text
                          (format nil "Constraints ~a failed on line ~s."
                                  (mapcar #'car cross-validated) line))))
               parsed-fields)))))))

(defun default-integer-parser (str &optional (start 0) end)
  "Returns an integer parsed from record STR between START and END."
  (let ((end (or end (length str))))
    (handler-case
        (parse-integer str :start start :end end)
      (parse-error (condition)
        (error 'malformed-record-error :text
               (format nil "~a" condition))))))

(defun default-float-parser (str &optional (start 0) end)
  "Returns a float parsed from record STR between START and END."
  (let ((end (or end (length str))))
    (handler-case
        (parse-float str :start start :end end)
      (parse-error (condition)
        (error 'malformed-record-error :text
               (format nil "~a" condition))))))

(defun parse-field (line start end parser &optional validator)
  "Returns a value parsed from LINE between START and END using
PARSER and VALIDATOR."
  (let ((parsed-value (funcall parser line start end)))
    (if (and validator (funcall validator parsed-value))
        parsed-value
      parsed-value)))

(defun cross-validate (name fields validator &rest field-names)
  "Returns a pair of constraint NAME and either T or NIL, indicating
the result of applying VALIDATOR to values from the alist of parsed
FIELDS named by FIELD-NAMES."
  (let ((field-values (mapcar #'(lambda (key)
                                  (assocdr key fields)) field-names)))
    (cons name (not (null (apply validator field-values))))))

(defun collect-parser-args (field)
  "Returns an argument list form for FIELD to be used by PARSE-FIELD
which has suitable parsers and validators set up for the standard
field types: :string, :integer and :float."
  (destructuring-bind (field-name &key ignore (type :string)
                                  parser validator)
      field
    (declare (ignore field-name))
    (if ignore
        `(list :ignore t)
      (let ((field-parser
             (or parser (ecase type
                          (:string #'subseq)
                          (:integer #'default-integer-parser)
                          (:float #'default-float-parser))))
            (field-validator
             (or validator (ecase type
                             (:string #'(lambda (s)
                                          (not (whitespace-string-p s))))
                             (:integer nil)
                             (:float nil)))))
        `(list ,field-parser ,field-validator)))))

(defun collect-constraint-args (form)
  (destructuring-bind (constraint-name field-names validator)
      form
    (declare (ignore constraint-name))
    `(list ,validator ,@(mapcar #'(lambda (n)
                                    `(quote ,n))
                                field-names))))