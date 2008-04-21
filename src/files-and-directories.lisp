;;;
;;; Copyright (C) 2007-2008, Keith James. All rights reserved.
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

(defparameter *default-tmpdir* "/tmp")

(defun make-tmp-pathname (&optional (tmpdir *default-tmpdir*)
                          (basename "") (suffix "tmp"))
  "Returns a pathname suitable for use as a temporary file. The
directory component of the new pathname is TMPDIR, defaulting to
*DEFAULT-TMPDIR*. The NAME component of the new pathname is a
concatenation of BASENAME, defaulting to an empty string, and a
pseudo-random number. The type component of the new pathname is
SUFFIX, defaulting to \"tmp\"."
  (unless (cl-fad:directory-exists-p tmpdir)
    (error 'invalid-argument-error
           :params 'tmpdir
           :args tmpdir
           :text "temporary file directory does not exist"))
  (merge-pathnames (cl-fad:pathname-as-directory tmpdir)
                   (make-pathname :directory '(:relative)
                                  :name (format nil "~a~a" basename
                                                (random most-positive-fixnum))
                                  :type suffix)))
