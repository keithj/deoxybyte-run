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

(defparameter *wait-for-run-process*
  #+:sbcl t
  #+:lispworks nil
  #-(or :sbcl :lispworks) t
  "Indicates whether the Lisp program should wait for the run
  process,or not.")

(defclass external-program ()
  ((shell :initform "sh"
          :initarg :shell
          :reader shell-of)
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

(defun run (command &key (non-zero-error t) (environment nil environmentp))
  (let ((program (apply #'make-instance 'external-program
                        :program "sh"
                        :args (list "-c" command)
                        :input :stream :output :stream :error :stream
                        :pty nil
                        :search t
                        :wait t
                        (when environmentp (list :environment environment)))))
    (unwind-protect
         (cond ((and (integerp (exit-code-of program)) 
                     (zerop (exit-code-of program)))
                program)
               (non-zero-error
                (error 'non-zero-exit-error :program program))
               (t
                nil))
      (close-process program)))) ; close or leak file descriptors

(defmethod print-object ((program external-program) stream)
  (with-accessors ((prg program-of) (args args-of) (exit-code exit-code-of))
      program
    (format stream "#<EXTERNAL-PROGRAM ~s ~{~^~s ~} exit: ~d>"
            prg args exit-code)))

#+:sbcl
(progn
  (defmethod initialize-instance :after ((program external-program)
                                         &rest process-args &key
                                         &allow-other-keys)
    (flet ((as-strings (alist)
             (loop
                for (key . val) in alist
                collect (format nil "~a=~a" key val))))
      (multiple-value-bind (args vals)
          (collect-key-values '(:input :output :error :if-input-does-not-exist
                                :if-output-exists :if-error-exists :environment
                                :search :pty :wait) process-args)
        (let ((proc-args (loop
                            for arg in args
                            for val in vals
                            nconc (if (eql :environment arg)
                                      (list arg (as-strings val))
                                    (list arg val)))))
          (setf (slot-value program 'process)
                ;; Run directly
                 (apply #'sb-ext:run-program (program-of program)
                        (args-of program)
                        proc-args)
                ;; Run inside another shell. This allows correct
                ;; handling of environment variables
;;                 (apply #'sb-ext:run-program (shell-of program)
;;                        (list "-c" (format nil "~a ~{~a~^ ~}"
;;                                           (program-of program)
;;                                           (args-of program))) proc-args)
                )))))

  (defmethod input-of ((program external-program))
    (sb-ext:process-input (process-of program)))

  (defmethod output-of ((program external-program))
    (sb-ext:process-output (process-of program)))

  (defmethod error-of ((program external-program))
    (sb-ext:process-error (process-of program)))

  (defmethod wait-for ((program external-program))
    (sb-ext:process-wait (process-of program) nil)) ; check-for-stopped nil

  (defmethod status-of ((program external-program))
    (sb-ext:process-status (process-of program)))

  (defmethod exit-code-of ((program external-program))
    (sb-ext:process-exit-code (process-of program)))

  (defmethod close-process ((program external-program))
    (sb-ext:process-close (process-of program)))

  (defmethod kill-process ((program external-program) signal
                           &optional (whom :pid))
    (sb-ext:process-kill (process-of program) signal whom)))

#+:lispworks
(progn
  (defstruct process
    (id 0)
    (input nil)
    (output nil)
    (error nil)
    (exit-code nil))
  
  (defmethod initialize-instance :after ((program external-program)
                                         &rest process-args &key
                                         &allow-other-keys)
    (multiple-value-bind (args vals)
        (collect-key-values '(:input :output :error :if-input-does-not-exist
                              :if-output-exists :if-error-exists :environment
                              :wait) process-args)
      (let ((proc-args
             (append (list :save-exit-status t)
                     (mapcar (lambda (keyword)
                               (case keyword
                                 (:error :error-output)
                                 (:if-error-exists :if-error-output-exists)
                                 (t keyword))) (loop
                                                  for arg in args
                                                  for val in vals
                                                  nconc (list arg val))))))
        (format t "~a ~a~%" args-of program) proc-args)
        (multiple-value-bind (stream error-stream pid)
            (apply #'system:run-shell-command
                   (format nil "~a -c '~a ~{~a~^ ~}'" (shell-of program)
                           (program-of program)
                           (args-of program)) proc-args)
          (setf (slot-value program 'process)
                (make-process :id pid :input stream :output stream
                              :error error-stream)))))
  
  (defmethod input-of ((program external-program))
    (process-input (process-of program)))

  (defmethod output-of ((program external-program))
    (process-output (process-of program)))

  (defmethod error-of ((program external-program))
    (process-error (process-of program)))

  (defmethod wait-for ((program external-program))
    (system:pid-exit-status (process-id (process-of program)) :wait t))

  (defmethod status-of ((program external-program))
    (if (null (system:pid-exit-status (process-id (process-of program))
                                      :wait nil))
        :running
      :exited))

  (defmethod exit-code-of ((program external-program))
    (system:pid-exit-status (process-id (process-of program)) :wait nil))

  (defmethod close-process ((program external-program))
    (let ((output (process-output (process-of program)))
          (error (process-error (process-of program))))
      (when (and output (open-stream-p output))
        (close output))
      (when (and error (open-stream-p error))
        (close error))))

  (defmethod kill-process ((program external-program) signal &optional whom)
    (declare (ignore program signal whom))
    (error "KILL-PROCESS is not supported on this platform.")))

#+:ccl
(progn
  (defmethod initialize-instance :after ((program external-program)
                                         &rest process-args &key
                                         &allow-other-keys)
    (multiple-value-bind (args vals)
        (collect-key-values '(:input :output :error :if-input-does-not-exist
                              :if-output-exists :if-error-exists :pty :wait)
                      process-args)
      (let ((proc-args (loop
                          for arg in args
                          for val in vals
                          nconc (list arg val))))
        (setf (slot-value program 'process)
              ;; Run directly
              (apply #'ccl:run-program (program-of program)
                     (args-of program)
                     proc-args)))))

  (defmethod input-of ((program external-program))
    (ccl:external-process-input-stream (process-of program)))

  (defmethod output-of ((program external-program))
    (ccl:external-process-output-stream (process-of program)))

  (defmethod error-of ((program external-program))
    (ccl:external-process-error-stream (process-of program)))

  (defmethod wait-for ((program external-program))
    (error "WAIT-FOR is not supported on this platform."))
  
  (defmethod status-of ((program external-program))
    (ccl:external-process-status (process-of program)))

  (defmethod exit-code-of ((program external-program))
    (multiple-value-bind (status exit-code)
        (process-of program)
      (declare (ignore status))
      exit-code))

  (defmethod close-process ((program external-program))
    (close (input-of program))
    (close (output-of program))
    (close (error-of program)))

  (defmethod kill-process ((program external-program) signal
                           &optional (whom :pid))
    (declare (ignore whom))
    (ccl:signal-external-process (process-of program) signal)))

(defmethod runningp ((program external-program))
  (eql :running (status-of program)))
