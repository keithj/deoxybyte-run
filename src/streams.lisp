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

;;; Gray-stream classes
(defclass wrapped-stream (fundamental-stream)
  ((stream :initarg :stream
           :reader stream-of
           :documentation "The underlying stream from which data are
read."))
  (:documentation "A Gray-stream wrapping a standard Lisp stream."))

(defclass stream-filter-mixin ()
  ((test :initarg :test
         :reader test-of
         :documentation "A function designator for a test that returns
T when the next datum read from the stream is to be ignored."))
  (:documentation "A mixin that provides a filtering function for
streams. Any data encountered while reading or writing for which the
test returns T are ignored and skipped."))
