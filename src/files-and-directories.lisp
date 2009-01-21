;;;
;;; Copyright (C) 2007-2009 Keith James. All rights reserved.
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

(defparameter *default-tmpdir* "/tmp"
  "The default temporary file directory.")

(defun make-tmp-pathname (&key (tmpdir *default-tmpdir*)
                          (basename "") (type "tmp"))
  "Returns a pathname suitable for use as a temporary file. The
directory component of the new pathname is TMPDIR, defaulting to
*DEFAULT-TMPDIR*. The NAME component of the new pathname is a
concatenation of BASENAME, defaulting to an empty string, and a
pseudo-random number. The type component of the new pathname is
TYPE, defaulting to \"tmp\"."
  (unless (cl-fad:directory-exists-p tmpdir)
    (error 'invalid-argument-error
           :params 'tmpdir
           :args tmpdir
           :text "temporary file directory does not exist"))
  (merge-pathnames (cl-fad:pathname-as-directory tmpdir)
                   (make-pathname :directory '(:relative)
                                  :name
                                  (format nil "~a~a" basename
                                          (random most-positive-fixnum))
                                  :type type)))

(defun make-pathname-gen (dir name &key type separator generator)
  "Returns a function of zero arity that generates pathnames when
called. The generated pathnames are relative to directory DIR and have
a namestring composed of NAME, SEPARATOR (defaults to NIL) and a value
taken from calling the function GENERATOR (defaults to a numeric
generator starting from 0, incrementing by 1). TYPE may be used to
specify the type of the new pathnames."
  (let ((g (or generator (make-number-gen))))
    (flet ((gen-pname (d n s g y)
             (merge-pathnames
              (fad:pathname-as-directory d)
              (make-pathname :directory '(:relative)
                             :name
                             (format nil "~a~@[~a~]~a" n s (next g))
                             :type y))))
      (lambda (op)
        (let ((current (gen-pname dir name separator g type)))
          (ecase op
            (:current current)
            (:next (prog1
                       current
                     (setf current (gen-pname dir name separator g type))))
            (:more t)))))))

(defun make-pathname-ext (pathname &key type separator generator)
  "Returns a function of arity 1 that returns modified copies of a
pathname argument. The pathname is modified by extending its
namestring. The new namestring is composed of the original namestring
SEPARATOR (defaults to NIL) and a value taken from calling the
function GENERATOR (defaults to a numeric generator starting from 0,
incrementing by 1). TYPE may be used to specify the type of the new
pathname, otherwise the original type will be used."
  (let ((g (or generator (make-number-gen))))
    (lambda ()
      (make-pathname :directory (pathname-directory pathname)
                     :name (format nil "~a~@[~a~]~a"
                                   (pathname-name pathname)
                                   separator (next g))
                     :type (or type (pathname-type pathname))))))
