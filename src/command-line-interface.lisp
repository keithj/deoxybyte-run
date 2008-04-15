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
"Parses a system command line to create a mapping of option keywords
to Lisp objects.

Example:

;;; (parse-command-line (list \"--sample-name\" \"sample one\")
;;;                     (list (cli-option
;;;                            :sname
;;;                            :name \"sample-name\"
;;;                            :argument-type :string
;;;                            :required-option t
;;;                            :documentation
;;;                             \"The sample name.\")))

Arguments:

- args (list string): a list of system command line arguments.
- options (list object): a list of command line options created by the
{defun cli-option} function.

Returns:

- parsed arguments from which argument values may be read using the
{defun cli-arg-value} function (alist).
- remaining arguments (list string).
- unknown arguments (list string)."
  (multiple-value-bind (remaining-args matched-args unmatched-args)
      (getopt:getopt args (getopt-options options))
    (when unmatched-args
      (warn-unmatched-args unmatched-args))
    (let ((parsed-args nil))
      (dolist (opt options)
        (let ((arg-value (assocdr (cli-opt-name opt)
                                  matched-args :test #'string=)))
          (cond ((and (cli-opt-required-p opt)
                      (null arg-value))
                 (error 'missing-required-option
                        :option (cli-opt-name opt)))
                ((cli-arg-parser opt)
                 (setf parsed-args
                       (acons (cli-opt-key opt)
                              (parse-value-safely opt arg-value)
                              parsed-args)))
              (t
               (setf parsed-args
                     (acons (cli-opt-key opt)
                            arg-value
                            parsed-args))))))
      (values parsed-args remaining-args unmatched-args))))

(defun print-help (help-message options
                   &optional (stream *error-output*))
  (format stream "~{~<~%~,70:;~a~> ~}~%"
          (loop for word in (split-sequence:split-sequence
                             #\Space help-message) collect word))
  (terpri stream)
  (write-line "  Options:" stream)
  (dolist (opt options)
    (print-option-help opt stream)
    (terpri stream)))

(defun cli-option (key &key name required-option required-argument
                   argument-type documentation)
  "Returns an object defining a command line interface option.

Example:

;;; (cli-option
;;;  :sname
;;;  :name \"sample-name\"
;;;  :argument-type :string
;;;  :required-option t
;;;  :documentation
;;;  \"The sample name.\")

Arguments:

- KEY (symbol): a key symbol by which the option will be known.
- :NAME (string): the full name of the option to be used on the
command line.
- :REQUIRED-OPTION (boolean): indicates whether the option is required
on the command line.
- :REQUIRED-ARGUMENT (boolean): indicates whether the option, if used,
requires an argument on the command line.
- :ARGUMENT-TYPE (symbol): indicates the type of argument accepted for
the option (:string :integer :float NIL).
- :DOCUMENTATION (string): a documentation string that may be printed
as command line help for the option.

Returns:
- A cli-option (list)."
  (when (and required-option (null argument-type))
    (error 'invalid-argument-error
           :params '(required-option argument-type)
           :args (list required-option argument-type)
           :text "this type is incompatible with a required argument"))
  (let ((argument-parser (ecase argument-type
                           (:string nil)
                           (:integer #'parse-integer)
                           (:float #'parse-float)
                           ((nil) nil))))
    (list key name required-option required-argument argument-type
          argument-parser documentation)))

(defun cli-opt-key (option)
  "Returns the key symbol for OPTION."
  (first option))

(defun cli-opt-name (option)
  "Returns the name of OPTION."
  (second option))

(defun cli-opt-required-p (option)
  "Returns T if OPTION is required on the command line, or NIL
otherwise."
  (third option))

(defun cli-arg-required-p (option)
  "Returns T if OPTION is requires an argument on the command line, or
NIL otherwise."
  (fourth option))

(defun cli-arg-type (option)
  "Returns the type of the argument of OPTION."
  (fifth option))

(defun cli-arg-parser (option)
  "Returns an argument parser function for OPTION that is capable of
parsing an argument string to the correct type."
  (sixth option))

(defun cli-opt-documentation (option)
  "Returns the command line documentation string for OPTION."
  (seventh option))

(defun cli-arg-value (option-key parsed-args)
  "Returns the value from PARSED-ARGS for the option named by the
symbol OPTION-KEY."
  (assocdr option-key parsed-args))

(defun print-option-help (option &optional (stream *error-output*))
  "Prints the help string for OPTION to STREAM (which defaults to
*ERROR-OUTPUT*)."
  (format stream "  --~15a <~a~:[~;, required~]>~%    ~a~%"
          (cli-opt-name option)
          (cli-arg-type option)
          (cli-opt-required-p option)
          (cli-opt-documentation option)))

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

(defun warn-unmatched-args (unmatched-args)
  "Prints a warning message to *ERROR-OUTPUT* describing
UNMATCHED-ARGS."
  (dolist (arg unmatched-args)
    (warn 'unknown-option :option arg)))

(defun parse-value-safely (option value)
  "Returns a parsed VALUE of the correct Lisp type for OPTION or
raises an {define-condition incompatible-argument} error."
  (handler-case
      (funcall (cli-arg-parser option) value)
    (parse-error (condition)
      (error 'incompatible-argument
             :option (cli-opt-name option)
             :type (cli-arg-type option)
             :argument value))))
