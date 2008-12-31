;;;
;;; Copyright (C) 2007-2008, Keith James. All rights reserved.
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

;;; General IO error conditions

(define-condition io-error (error)
  ()
  (:documentation "The parent type of all IO error conditions."))

(define-condition io-warning (warning)
  ()
  (:documentation "The parent type of all IO warning conditions."))


;;; Command line interface conditions
(define-condition cli-error (error)
  ()
  (:documentation "The parent type of all CLI error conditions."))

(define-condition cli-warning (warning)
  ()
  (:documentation "The parent type of all CLI warning conditions."))

(define-condition unknown-command (cli-error)
  ((command :initarg :command
            :reader command-of
            :documentation "The unknown command."))
  (:report (lambda (condition stream)
             (format stream "Unknown command ~a."
                     (command-of condition))))
  (:documentation "An error that is raised when the main command is
not recognised."))

(define-condition missing-required-option (cli-error)
  ((option :initarg :option
           :reader option-of
           :documentation "The missing option."))
  (:report (lambda (condition stream)
             (format stream "Missing required option --~a."
                     (option-of condition))))
  (:documentation "An error that is raised when a required option is
missing."))

(define-condition incompatible-argument (cli-error)
  ((option :initarg :option
           :reader option-of
           :documentation "The option for which the bad argument was
supplied.")
   (type :initarg :type
         :reader type-of
         :documentation "The expected type of option argument.")
   (argument :initarg :argument
             :reader argument-of
             :documentation "The bad argument."))
  (:report (lambda (condition stream)
             (format stream "Invalid argument ~a supplied for option --~a (~a)."
                     (argument-of condition)
                     (option-of condition) 
                     (type-of condition))))
  (:documentation "An error that is raised when an option is supplied
with an argument of the wrong type."))

(define-condition unmatched-option (cli-warning)
  ((option :initarg :option
           :reader option-of
           :documentation "The option for which the bad argument was
supplied."))
  (:report (lambda (condition stream)
             (format stream "unmatched argument --~a"
                     (option-of condition)))))

(define-condition unknown-option (cli-warning)
  ((option :initarg :option
           :reader option-of
           :documentation "The option for which the bad argument was
supplied."))
  (:report (lambda (condition stream)
             (format stream "unknown argument --~a"
                     (option-of condition)))))


;;; Parse conditions
(define-condition general-parse-error (io-error)
  ((text :initform nil
         :initarg :text
         :reader text-of
         :documentation "Error message text."))
  (:report (lambda (condition stream)
             (format stream "General parse error~@[: ~a~]"
                     (text-of condition))))
  (:documentation "The parent type of all parse error conditions."))

(define-condition malformed-record-error (general-parse-error)
  ((record :initform nil
           :initarg :record
           :reader record-of
           :documentation "The malformed record."))
  (:report (lambda (condition stream)
             (format stream "Malformed record error~@[: ~a~]"
                     (text-of condition))))
  (:documentation "An error that is raised when a record is malformed
for any reason."))

(define-condition record-validation-error (malformed-record-error)
  ()
  (:report (lambda (condition stream)
             (format stream "Record validation error~@[: ~a~]"
                     (text-of condition))))
  (:documentation "An error that is raised when a record fails
validation of one or more of its parts."))

(define-condition malformed-field-error (malformed-record-error)
  ((field :initform nil
          :initarg :field
          :reader field-of
          :documentation "The malformed field."))
  (:report (lambda (condition stream)
             (format stream "Malformed field error~@[: ~a~]"
                     (text-of condition))))
  (:documentation "An error that is raised when a field-based record
contains a malformed field within it."))

(define-condition field-validation-error (malformed-field-error)
  ()
  (:report (lambda (condition stream)
             (format stream "Field validation error~@[: ~a~]"
                     (text-of condition))))
  (:documentation "An error that is raised when a record field fails
validation."))
