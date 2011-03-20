;;;
;;; Copyright (c) 2008-2011 Keith James. All rights reserved.
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

(defparameter *wait-for-run-process*
  #+:sbcl t
  #+:lispworks nil
  #+:ccl t
  #-(or :sbcl :lispworks :ccl) t
  "Indicates whether the Lisp program should wait for the run
  process,or not.")

(defvar *program-instances-mutex*
  #+:sbcl (sb-thread:make-mutex :name "program instances lock")
  #+:lispworks (mp:make-lock)
  #+:ccl (ccl:make-lock "program instances lock")
  "Mutex lock that protects the list of running {defclass external-program}
instances.")

(defparameter *program-instances* ()
  "List where running {defclass external-program} instances are
recorded")

;;; Using a third-party compatability library is overkill for this
;;; single locking case.

#+:sbcl
(defmacro with-lock ((mutex) &body body)
  "Executes BODY with MUTEX lock held."
  `(sb-thread:with-mutex (,mutex)
     ,@body))

#+:lispworks
(defmacro with-lock ((mutex) &body body)
  "Executes BODY with MUTEX lock held."
  `(mp:with-lock (,mutex)
     ,@body))

#+:ccl
(defmacro with-lock ((mutex) &body body)
  "Executes BODY with MUTEX lock held."
  `(ccl:with-lock-grabbed (,mutex)
     ,@body))

(defun update-program-instances (&optional program)
  "Updates the list of all currently running {defclass
external-program} instances, adding an optional instance PROGRAM and
removing any instances that are no longer running. Returns the list of
the currently running program instances."
  (with-lock (*program-instances-mutex*)
    (when (and program (runningp program))
      (push program *program-instances*))
    (setf *program-instances* (delete-if (complement #'runningp)
                                         *program-instances*))))

(defun evict-program-instance (program)
  "Specifically removes PROGRAM from the list of all currently running
{defclass external-program} instances. Returns the list of the
currently running program instances."
  (with-lock (*program-instances-mutex*)
    (setf *program-instances* (delete program *program-instances*))))

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

(defun programs-running ()
  "Returns a list of all currently running {defclass external-program}
instances."
  (with-lock (*program-instances-mutex*)
    (remove-if (complement #'runningp) *program-instances*)))

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
  (let ((program (apply #'make-instance 'external-program
                        :program "sh"
                        :args (list "-c" command)
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
                (error 'non-zero-exit-error :program program))
               (t
                nil))
      (close-process program)))) ; close or leak file descriptors

(defmethod print-object ((program external-program) stream)
  (with-accessors ((prg program-of) (args args-of) (exit-code exit-code-of))
      program
    (format stream "#<EXTERNAL-PROGRAM ~s ~{~^~s ~} exit: ~d>"
            prg args exit-code)))

(defmethod kill-process :after ((program external-program) signal
                                &optional whom)
  (declare (ignore signal whom))
  (evict-program-instance program))

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
                 (apply #'sb-ext:run-program (program-of program)
                        (args-of program)
                        proc-args))
          (update-program-instances program)))))

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
        (multiple-value-bind (stream error-stream pid)
            (apply #'system:run-shell-command
                   (format nil "~a -c '~a ~{~a~^ ~}'" (shell-of program)
                           (program-of program)
                           (args-of program)) proc-args)
          (setf (slot-value program 'process)
                (make-process :id pid :input stream :output stream
                              :error error-stream))
          (update-program-instances program)))))
  
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
                     proc-args))
        (update-program-instances program))))

  (defmethod input-of ((program external-program))
    (ccl:external-process-input-stream (process-of program)))

  (defmethod output-of ((program external-program))
    (ccl:external-process-output-stream (process-of program)))

  (defmethod error-of ((program external-program))
    (ccl:external-process-error-stream (process-of program)))

  (defmethod wait-for ((program external-program))
    nil)
  
  (defmethod status-of ((program external-program))
    (ccl:external-process-status (process-of program)))

  (defmethod exit-code-of ((program external-program))
    (multiple-value-bind (status exit-code)
        (ccl:external-process-status (process-of program))
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
