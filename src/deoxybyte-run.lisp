;;;
;;; Copyright (C) 2008, 2009, 2010, 2011, 2012, 2015 Keith James. All
;;; rights reserved.
;;;
;;; This file is part of deoxybyte-run.
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

(in-package :uk.co.deoxybyte-run)

(defparameter *wait-for-run-process* t
  "Indicates whether the Lisp program should wait for the run
  process, or not.")

(defclass external-program ()
  ((program :initarg :program
            :reader program-of
            :documentation "The name of the external program to be
executed.")
   (args :initform nil
         :initarg :args
         :reader args-of
         :documentation "A list of default argument strings passed to
the program when its process is started.")
   (process :initform nil
            :reader process-of
            :documentation "The process of the external program."))
  (:documentation "Instances of EXTERNAL-PROGRAM represent the
processes of external processes launched by Lisp. The main purposes of
this class are to standardise handling of input, output and error
streams, such that they are always available via accessors and to
allow creation of subclasses that handle these streams in defined
ways."))

(defgeneric start-process (program &rest process-args)
  (:documentation "Start the external process running PROGRAM."))

(defgeneric startedp (program)
  (:documentation "Return T if the external process has been started,
or NIL otherwise."))

(defgeneric input-of (program)
  (:documentation "Return the input stream of PROGRAM."))

(defgeneric output-of (program)
  (:documentation "Return the output stream of PROGRAM."))

(defgeneric error-of (program)
  (:documentation "Return the error stream of PROGRAM."))

(defgeneric wait-for (program)
  (:documentation "Wait for PROGRAM to exit."))

(defgeneric status-of (program)
  (:documentation "Return a keyword indicating the status of
PROGRAM. One of :running :stopped :signaled or :exited ."))

(defgeneric exit-code-of (program)
  (:documentation "Return an integer exit code of PROGRAM, or NIL if
the exit code is not available."))

(defgeneric close-process (program)
  (:documentation "Close the input, output and error streams of
PROGRAM."))

(defgeneric kill-process (program signal &optional whom)
  (:documentation ""))

(defgeneric runningp (program)
  (:documentation "Return T if PROGRAM is running, or NIL otherwise."))

(defmethod print-object ((program external-program) stream)
  (with-accessors ((prg program-of) (args args-of) (exit-code exit-code-of))
      program
    (format stream "#<EXTERNAL-PROGRAM ~s ~{~^~s ~} exit: ~d>"
            prg args exit-code)))

(defun run (command &key (non-zero-error t) (environment nil environmentp))
  "This is a convenience function that runs an external program
specified by a pre-formatted shell command string.

Arguments:

- command (string): A shell command.

Key:

- non-zero-error (boolean): If T will raise a
  {define-condition non-zero-exit-error} if the external program exits
  with a non-zero exit code. The default is T.
- environment (alist): An alist of keys and values describing the
  environment

Returns:

- An instance of {defclass external-program}"
  (let ((program
         (apply #'start-process
                (make-instance 'external-program :program "sh"
                               :args (list "-c" command))
                :input :stream :output :stream :error :stream
                :pty nil
                :search t
                :wait t
                (when environmentp
                  (list :environment environment)))))
    (unwind-protect
         (cond ((and (integerp (exit-code-of program))
                 (zerop (exit-code-of program)))
                program)
               (non-zero-error
                (error 'non-zero-exit-error :program program
                       :exit-code (exit-code-of program)))
               (t
                nil))
      (close-process program)))) ; close or leak file descriptors
