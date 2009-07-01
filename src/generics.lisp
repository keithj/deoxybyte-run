;;;
;;; Copyright (C) 2009 Keith James. All rights reserved.
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
