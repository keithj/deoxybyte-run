;;;
;;; Copyright (c) 2012 Keith James. All rights reserved.
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

;;; deoxybyte-run

(defvar *program-instances-mutex*
  (sb-thread:make-mutex :name "program instances lock")
  "Mutex lock that protects the list of running {defclass external-program}
instances.")

;;; Using a third-party compatability library is overkill for this
;;; single locking case.
(defmacro with-lock ((mutex) &body body)
  "Executes BODY with MUTEX lock held."
  `(sb-thread:with-mutex (,mutex)
     ,@body))

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
  (sb-ext:process-kill (process-of program) signal whom))

(defmethod runningp ((program external-program))
  (eql :running (status-of program)))
