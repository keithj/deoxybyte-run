;;;
;;; Copyright (C) 2008-2009 Keith James. All rights reserved.
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

(defparameter *list-separator-char* #\,
  "The separator character used in multi-value arguments.")

(defmacro with-argv (argv &body body)
  `(let ((,argv (get-system-argv)))
    ,@body))

(defmacro with-backtrace ((&key quit error-file) &body body)
  `(handler-bind
    ((error (lambda (condition)
              (format *error-output* "~a~%" condition)
              ,(if error-file
                   `(with-open-file (stream ,error-file
                                     :direction :output
                                     :if-does-not-exist :create
                                     :if-exists :append)
                     (print-backtrace condition stream))
                   '(print-backtrace condition *error-output*))
              ,(when quit
                     '(quit-lisp)))))
    ,@body))

(defmacro with-cli ((argv &key quit error-file) &body body)
  `(with-backtrace (:quit ,quit ,@(when error-file
                                        `(:error-file ,error-file)))
    (with-argv ,argv
          ,@body)))

#+:sbcl
(defun get-system-argv ()
  (rest sb-ext:*posix-argv*))

#+:lispworks
(defun get-system-argv ()
  (rest *line-arguments-list*))

#-(or :sbcl :lispworks)
(defun get-system-argv ()
  (error "Not implemented on ~a" (lisp-implementation-type)))

#+:sbcl
(defun print-backtrace (condition stream)
  (declare (ignore condition))
  (sb-debug:backtrace 20 stream))

#+:lispworks
(defun print-backtrace (condition stream)
  (declare (ignore condition))
  (let ((*debug-io* stream))
    (dbg:with-debugger-stack ()
      (dbg:bug-backtrace nil))))

#-(or :sbcl :lispworks)
(defun print-backtrace (condition)
  (error "Not implemented on ~a" (lisp-implementation-type)))

#+:sbcl
(defun quit-lisp ()
  (sb-ext:quit))

#+:lispworks
(defun quit-lisp ()
  (lw:quit))

#-(or :sbcl :lispworks)
(defun quit-lisp ()
  (error "Not implemented on ~a" (lisp-implementation-type)))

(defun parse-command-line (args options)
  "Parses a system command line to create a mapping of option keywords
to Lisp objects. Where multiple values are to be accepted for an
argument e.g. :integer-list , they must be comma-separated on the
command line e.g. 1,2,3,4.

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
      (warn-unmatched-args unmatched-args options))
    (let ((parsed-args nil))
      (dolist (opt options)
        (let ((arg-value (assocdr (cli-opt-name opt)
                                  matched-args :test #'string=)))
          (cond ((and (cli-opt-required-p opt) ; missing opts
                      (null arg-value))
                 (error 'missing-required-option
                        :option (cli-opt-name opt)))
                ((and (cli-arg-required-p opt) ; opt parseable args
                      (cli-arg-parser opt))
                   (let ((parsed-arg (if (null arg-value)
                                         arg-value
                                       (parse-value-safely opt arg-value))))
                     (setf parsed-args
                           (acons (cli-opt-key opt)
                                  parsed-arg
                                  parsed-args))))
                ((and (not (cli-opt-required-p opt)) ; boolean flags
                      (not (cli-arg-required-p opt))
                      (assoc (cli-opt-name opt)
                             matched-args :test #'string=))
                 (setf parsed-args
                       (acons (cli-opt-key opt)
                              '(t)
                              parsed-args)))
                (t ; plain strings
                 (setf parsed-args
                       (acons (cli-opt-key opt)
                              arg-value
                              parsed-args))))))
      (values parsed-args remaining-args unmatched-args))))

(defun print-cli-help (help-message options
                       &optional (stream *error-output*))
  "Prints a help message and help for each avaliable option.

 Arguments:

 - help-message (string): a help message.
 - options (list object): a list of command line options created by the
 {defun cli-option} function from which the individual option
 documentation will be extracted for printing.

 Optional:

 - stream (stream): the stream to which the message will be printed.
 Defaults to *ERROR-OUTOUT*.

 Returns:

 - T."
  (format stream "~{~<~%~,70:;~a~> ~}~%"
          (loop for word in (split-sequence:split-sequence
                             #\Space help-message) collect word))
  (terpri stream)
  (write-line "  Options:" stream)
  (dolist (opt options)
    (print-option-help opt stream)
    (terpri stream))
  t)

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

- key (symbol): a key symbol by which the option will be known.

Key:

- :name (string): the full name of the option to be used on the
command line.

- :required-option (boolean): indicates whether the option is required
on the command line.
- :required-argument (boolean): indicates whether the option, if used,
requires an argument on the command line.
- :argument-type (symbol): indicates the type of argument accepted for
the option (:string :character :integer :float :string-list
:character-list :integer-list :float-list NIL).
- :documentation (string): a documentation string that may be printed
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
                           (:character #'parse-character)
                           (:integer #'parse-integer)
                           (:float #'parse-float)
                           (:string-list #'parse-string-list)
                           (:character-list #'parse-character-list)
                           (:integer-list #'parse-integer-list)
                           (:float-list #'parse-float-list)
                           ((nil) nil)))) ; boolean
    (list key name required-option required-argument argument-type
          argument-parser documentation)))

(defun cli-opt-p (name options)
  (find name options :key #'cli-opt-name :test #'string=))

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
  (unless (cli-key-present-p option-key parsed-args)
    (error 'invalid-argument-error
           :params '(option-key parsed-args)
           :args (list option-key parsed-args)
           :text "there is no value for this key in the parsed arguments"))
  (assocdr option-key parsed-args))

(defun cli-key-present-p (option-key parsed-args)
  "Returns T if a value for OPTION-KEY is present in PARSED-ARGS."
  (member option-key parsed-args :key #'first))

(defun print-option-help (option &optional (stream *error-output*))
  "Prints the help string for OPTION to STREAM (which defaults to
*ERROR-OUTPUT*)."
  (format stream "  --~15a <~@[~a, ~]~:[optional~;required~]>~%    ~a~%"
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
    (mapcar (lambda (opt)
              (list (cli-opt-name opt)
                    (getopt-keyword opt))) options)))

(defun warn-unmatched-args (unmatched-args options)
  "Prints a warning message to *ERROR-OUTPUT* describing
UNMATCHED-ARGS."
  (dolist (arg unmatched-args)
    (if (cli-opt-p arg options)
        (warn 'unmatched-option :option arg)
      (warn 'unknown-option :option arg))))

(defun parse-value-safely (option value)
  "Returns a parsed VALUE of the correct Lisp type for OPTION or
raises an {define-condition incompatible-argument} error."
  (handler-case
      (funcall (cli-arg-parser option) value)
    (parse-error (condition)
      (declare (ignore condition))
      (error 'incompatible-argument
             :option (cli-opt-name option)
             :type (cli-arg-type option)
             :argument value))))

(defun parse-character (string)
  "Returns a character parsed from STRING of length 1 character."
  (let ((trimmed (string-trim '(#\Space) string)))
    (if (= 1 (length trimmed))
        (char trimmed 0)
      (error 'parse-error "expected a string of one character"))))

(defun parse-string-list (string)
  "Returns a list of strings parsed from STRING by splitting on the
*list-separator-char* character."
  (split-sequence:split-sequence *list-separator-char* string
                                 :remove-empty-subseqs t))

(defun parse-integer-list (string)
  "Returns a list of integers parsed from STRING after splitting on
the *list-separator-char* character."
  (mapcar #'parse-integer
          (split-sequence:split-sequence *list-separator-char* string
                                         :remove-empty-subseqs t)))

(defun parse-character-list (string)
  "Returns a list of integers parsed from STRING after splitting on
the *list-separator-char* character."
  (mapcar #'parse-character
          (split-sequence:split-sequence *list-separator-char* string
                                         :remove-empty-subseqs t)))

(defun parse-float-list (string)
  "Returns a list of floats parsed from STRING after splitting on the
*list-separator-char* character."
  (mapcar #'parse-float
          (split-sequence:split-sequence *list-separator-char* string
                                         :remove-empty-subseqs t)))
