;;;
;;; Copyright (c) 2008-2012 Keith James. All rights reserved.
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
  process,or not.")

(defclass external-program ()
  ((shell :initform "sh"
          :initarg :shell
          :reader shell-of
          :documentation "The shell under which to execute the
external program.")
   (program :initarg :program
            :reader program-of
            :documentation "The name of the external program to be
executed.")
   (args :initarg :args
         :reader args-of
         :documentation "A list of default argument strings passed to
the program when its process is started.")
   (process :reader process-of
            :documentation "The process of the external program."))
  (:documentation "Instances of EXTERNAL-PROGRAM represent the
processes of external processes launched by Lisp. The main purposes of
this class are to standardise handling of input, output and error
streams, such that they are always available via accessors and to
allow creation of subclasses that handle these streams in defined
ways."))

(defgeneric input-of (program)
  (:documentation "Returns the input stream of PROGRAM."))

(defgeneric output-of (program)
  (:documentation "Returns the output stream of PROGRAM."))

(defgeneric error-of (program)
  (:documentation "Returns the error stream of PROGRAM."))

(defgeneric wait-for (program)
  (:documentation "Waits for PROGRAM to exit."))

(defgeneric status-of (program)
  (:documentation "Returns keyword indicating the status of
PROGRAM. One of :running :stopped :signaled or :exited ."))

(defgeneric exit-code-of (program)
  (:documentation "Returns an integer exit code of PROGRAM, or NIL if
the exit code is not available."))

(defgeneric close-process (program)
  (:documentation "Closes the input, output and error streams of
PROGRAM."))

(defgeneric kill-process (program signal &optional whom)
  (:documentation ""))

(defgeneric runningp (program)
  (:documentation "Returns T if PROGRAM is running, or NIL otherwise."))

(defmethod print-object ((program external-program) stream)
  (with-accessors ((prg program-of) (args args-of) (exit-code exit-code-of))
      program
    (format stream "#<EXTERNAL-PROGRAM ~s ~{~^~s ~} exit: ~d>"
            prg args exit-code)))

