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

(defclass external-program ()
  ((program :initarg :program
            :reader program-of
            :documentation "The name of the external program to be
executed. The program must be on the current PATH.")
   (args :initarg :args
         :reader args-of
         :documentation "A list of default argument strings passed to
the program when its process is started.")
   (process :accessor process-of
            :documentation "The process of the external program.")
   (input-stream :accessor input-stream-of
                 :documentation "The input stream of the external
program.")
   (output-stream :accessor output-stream-of
                  :documentation "The output stream of the external
program.")
   (error-stream :accessor error-stream-of
                 :documentation "The error stream of the external
program."))
  (:documentation "Instances of EXTERNAL-PROGRAM represent the
processes of external processes launched by Lisp. The main purposes of
this class are to standardise handling of input, output and error
streams, such that they are always available via accessors and to
allow creation of subclasses that handle these streams in defined
ways."))

#+sbcl
(defmethod initialize-instance :after ((program external-program)
                                       &key debug)
  (let ((process (sb-ext:run-program (program-of program) (args-of program)
                                     :wait nil
                                     :pty nil
                                     :search t
                                     :input :stream
                                     :output :stream
                                     :error :stream)))
    (setf (process-of program) process
          (input-stream-of program) (if debug
                                        (make-broadcast-stream
                                         *standard-output*
                                         (sb-ext:process-input process))
                                      (sb-ext:process-input process))
          (output-stream-of program) (sb-ext:process-output process)
          (error-stream-of program) (sb-ext:process-error process))))
