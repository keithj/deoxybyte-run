;;;
;;; Copyright (C) 2008 Keith James. All rights reserved.
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


(defun parse-command-line (args options)
  "
 Purpose: Parses a system command line to create a mapping of option
          keywords to Lisp objects.

    args: ARGS - a list of strings
          OPTIONS - a list of command line options created by the
          CLI-OPTION function.

 Returns: An alist of parsed arguments from which argument values
          may be read using the ARG-VALUE function.
          A list of remaining arguments.
          A list of unknown arguments.
"
  (multiple-value-bind (remaining-args matched-args unknown-args)
      (getopt:getopt args (getopt-options options))
    (when unknown-args
      (warn-unknown-args unknown-args))
    (let ((parsed-args nil))
      (dolist (opt options)
        (let ((arg-value (assocdr (cli-opt-name opt)
                                  matched-args :test #'string=)))
          (cond ((and (cli-opt-required-p opt)
                      (null arg-value))
                 (error "Option ~a is required." (cli-opt-name opt)))
                ((cli-arg-parser opt)
                 (setf parsed-args
                       (acons (cli-opt-keyword opt)
                              (funcall (cli-arg-parser opt) arg-value)
                              parsed-args)))
              (t
               (setf parsed-args
                     (acons (cli-opt-keyword opt)
                            arg-value
                            parsed-args))))))
      (values parsed-args remaining-args unknown-args))))


(defun cli-option (keyword &key name required-option required-argument
                   argument-type documentation)
  (when (and required-option (null argument-type))
    (error (msg "The required option --~a is incompatible"
                "with an argument type of ~a.")
           name argument-type))
  (let ((argument-parser (ecase argument-type
                           (:string nil)
                           (:integer #'parse-integer)
                           (:float #'parse-float)
                           ((nil) nil))))
    (list keyword name required-option required-argument
          argument-type argument-parser documentation)))

(defun cli-opt-keyword (option)
  (first option))

(defun cli-opt-name (option)
  (second option))

(defun cli-opt-required-p (option)
  (third option))

(defun cli-arg-required-p (option)
  (fourth option))

(defun cli-arg-type (option)
  (fifth option))

(defun cli-arg-parser (option)
  (sixth option))

(defun cli-opt-documentation (option)
  (seventh option))

(defun cli-arg-value (option-keyword parsed-args)
  "Returns the value from PARSED-ARGS for the option named by the
keyword OPTION-KEYWORD."
  (assocdr option-keyword parsed-args))

(defun getopt-options (options)
  (flet ((getopt-keyword (opt)
           (cond ((cli-arg-required-p opt)
                  :required)
                 ((null (cli-arg-type opt))
                  :none)
                 (t
                  :optional))))
    (mapcar #'(lambda (opt)
                (list (cli-opt-name opt)
                      (getopt-keyword opt))) options)))

(defun warn-unknown-args (unknown-args)
  "Prints a warning message to *ERROR-OUTPUT* describing
UNKNOWN-ARGS."
  (format *error-output* "Warning: unknown arguments ~a."
          (mapcar #'(lambda (a)
                      (format nil "--~a" a)) unknown-args)))
