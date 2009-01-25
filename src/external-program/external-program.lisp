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

(defclass external-program ()
  ((program :initarg :program
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

(defstruct process
  (id 0)
  (input nil)
  (output nil)
  (error nil)
  (exit-code nil))

#+sbcl
(defmethod initialize-instance :after ((program external-program)
                                       &rest process-args &key)
  (let ((proc-args (remove-args '(:echo :program :args) process-args)))
    (setf (slot-value program 'process)
          (apply #'sb-ext:run-program
                 (program-of program) (args-of program) proc-args))))

#+lispworks
(defmethod initialize-instance :after ((program external-program)
                                       &rest process-args &key)
  (let ((proc-args (remove-args '(:echo :search :program :args) process-args)))
    (multiple-value-bind (stream error-stream pid)
        (apply #'system:run-shell-command
               (format nil "sh -c '~a ~{~a~^ ~}'"
                       (program-of program) (args-of program)) proc-args)
      (setf (slot-value program 'process)
            (make-process :id pid :input stream :output stream
                          :error error-stream)))))

#+sbcl
(defmethod input-of ((program external-program))
  (sb-ext:process-input (process-of program)))

#+lispworks
(defmethod input-of ((program external-program))
  (process-input (process-of program)))

#+sbcl
(defmethod output-of ((program external-program))
  (sb-ext:process-output (process-of program)))

#+lispworks
(defmethod output-of ((program external-program))
  (process-output (process-of program)))

#+sbcl
(defmethod error-of ((program external-program))
  (sb-ext:process-error (process-of program)))

#+lispworks
(defmethod error-of ((program external-program))
  (process-error (process-of program)))

#+sbcl
(defmethod wait-for ((program external-program) &optional check-for-stopped)
  (sb-ext:process-wait (process-of program) check-for-stopped))

#+lispworks
(defmethod wait-for ((program external-program) &optional check-for-stopped)
 (declare (ignore program check-for-stopped)))

#+sbcl
(defmethod status-of ((program external-program))
  (sb-ext:process-status (process-of program)))

#+lispworks
(defmethod status-of ((program external-program))
  nil)

#+sbcl
(defmethod exit-code-of ((program external-program))
  (sb-ext:process-exit-code (process-of program)))

#+lispworks
(defmethod exit-code-of ((program external-program))
  (system:pid-exit-status (process-id (process-of program))))

#+sbcl
(defmethod close-process ((program external-program))
  (sb-ext:process-close (process-of program)))

#+lispworks
(defmethod close-process ((program external-program))
  (let ((output (process-output (process-of program)))
        (error (process-error (process-of program))))
    (when (and output (open-stream-p output))
      (close output))
    (when (and error (open-stream-p error))
      (close error))))

#+sbcl
(defmethod kill-process ((program external-program) signal
                         &optional (whom :pid))
  (sb-ext:process-kill (process-of program) signal whom))

#+lispworks
(defmethod kill-process ((program external-program) signal
                         &optional whom)
  (declare (ignore program signal whom)))

(defun run-program (program args &rest initargs)
  (apply #'make-instance 'external-program :program program :args args
         :allow-other-keys t initargs))

