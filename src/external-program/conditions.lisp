;;;
;;; Copyright (C) 2009 Keith James. All rights reserved.
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

(in-package :uk.co.deoxybyte-io)

(define-condition external-program-error (error)
  ((text :initform nil
         :initarg :text
         :reader text-of
         :documentation "Error message text."))
  (:documentation "The parent type of all external program error
  conditions."))

(define-condition non-zero-exit-error (external-program-error)
  ((program :initform nil
            :initarg :program
            :reader program-of
            :documentation "The object that failed to return a zero
            exit code.")
   (exit-code :initform nil
              :initarg :exit-code
              :reader exit-code-of
              :documentation "The exit code."))
  (:report (lambda (condition stream)
             (format stream "Non-zero exit code~@[ ~a~] from ~a ~@[: ~a~]"
                     (exit-code-of condition)
                     (program-of condition)
                     (text-of condition))))
  (:documentation "An error that is raised when an external program
  returns a non-zero exit code."))
