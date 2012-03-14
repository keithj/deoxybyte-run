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

(defparameter *program-instances* ()
  "List where running {defclass external-program} instances are
recorded")

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

(defun programs-running ()
  "Returns a list of all currently running {defclass external-program}
instances."
  (with-lock (*program-instances-mutex*)
    (remove-if (complement #'runningp) *program-instances*)))

(defmethod kill-process :after ((program external-program) signal
                                &optional whom)
  (declare (ignore signal whom))
  (evict-program-instance program))
