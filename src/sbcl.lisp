;;;
;;; Copyright (C) 2012, 2015 Keith James. All rights reserved.
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

(defmethod start-process ((program external-program) &rest process-args
                          &key &allow-other-keys)
  (flet ((as-strings (alist)
           (loop
              for (key . val) in alist
              collect (format nil "~a=~a" key val))))
    (multiple-value-bind (args vals)
        (collect-key-values '(:input :output :error :if-input-does-not-exist
                              :if-output-exists :if-error-exists :environment
                              :search :pty :wait :status-hook) process-args)
      (let ((proc-args (loop
                          for arg in args
                          for val in vals
                          nconc (if (eql :environment arg)
                                    (list arg (as-strings val))
                                    (list arg val)))))
        (setf (slot-value program 'process)
              (apply #'sb-ext:run-program (program-of program) (args-of program)
                     proc-args)))))
  program)

(defmethod startedp ((program external-program))
  (process-of program))

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
  (and (process-of program) (sb-ext:process-exit-code (process-of program))))

(defmethod close-process ((program external-program))
  (sb-ext:process-close (process-of program)))

(defmethod kill-process ((program external-program) signal
                         &optional (whom :pid))
  (sb-ext:process-kill (process-of program) signal whom))

(defmethod runningp ((program external-program))
  (and (startedp program) (eql :running (status-of program))))

(defmethod finishedp ((program external-program))
  (and (startedp program) (member (status-of program) '(:exited :signaled))))

(defun cleanup-process (process)
  (when (eql :exited (sb-ext:process-status process))
    (sb-ext:process-close process)))
