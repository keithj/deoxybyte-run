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

(in-package :cl-io-utilities)

(defparameter *rsh-executable* "rsh" "The rsh executable name.")
(defparameter *ls-executable* "ls" "The ls executable name.")
(defparameter *mkdir-executable* "mkdir" "The mkdir executable name.")

(defparameter *wait-for-rsh-process*
  #+sbcl t
  #+lispworks nil
  #-(or sbcl lispworks) t
  "Indicates whether Lisp program should wait for the rsh process, or
  not.")

(defclass rsh (external-program)
  ((host :initarg :host
         :reader host-of
         :documentation "The remote host on which the command is
         executed."))
  (:documentation "An instance of this class represents an rsh
  process."))

(defmethod print-object ((rsh rsh) stream)
  (with-accessors ((host host-of) (args args-of) (exit-code exit-code-of))
      rsh
    (format stream "#<RSH host:~a command: ~a exit: ~d>" host (rest args)
            exit-code)))

(defun rsh-exec (host &rest command)
  "Executes COMMAND on HOST using rsh/ssh and returns an instance of
defclass rsh . If the command returns a non-zero exit code, a
NON-ZERO-EXIT-ERROR error is raised."
  (let ((rsh (make-instance 'rsh :program *rsh-executable*
                                 :host host :args (cons host command)
                                 :input :stream :output :stream
                                 :search t :wait *wait-for-rsh-process*
                                 :allow-other-keys t)))
    (if (zerop (exit-code-of rsh))
        rsh
      (error 'non-zero-exit-error :program rsh))))

(defun rsh-list-files (host pathspec &optional (ignore-backups t))
  "Returns a list of namestrings of files in directory PATHSPEC on
HOST. If IGNORE-BACKUPS is T (the default) then tilda backup files are
ignored."
  (loop
     for filename in (rsh-list host pathspec ignore-backups)
     unless (ends-with-char-p filename #\/)
     collect filename))

(defun rsh-list-directories (host pathspec)
  "Returns a list of namestrings of subdirectories in directory
PATHSPEC on HOST."
  (loop
     for filename in (rsh-list host pathspec)
     when (ends-with-char-p filename #\/)
     collect (string-right-trim '(#\/) filename)))

(defun rsh-file-exists-p (host pathspec filespec)
  "Returns T if file FILESPEC exists in directory PATHSPEC on HOST, or
NIL otherwise."
  (rsh-pathname-exists-p host pathspec filespec #'rsh-list-files))

(defun rsh-directory-exists-p (host pathspec1 pathspec2)
  "Returns T if subdirectory PATHSPEC1 exists in directory PATHSPEC2
on HOST, or NIL otherwise."
  (rsh-pathname-exists-p host pathspec1 pathspec2 #'rsh-list-directories))

(defun rsh-files-exist-p (host pathspec &rest filespecs)
  "Returns T if all of the files FILESPECS exist in directory PATHSPEC
on HOST, or NIL otherwise."
  (apply #'rsh-pathnames-exist-p host pathspec #'rsh-list-files
         filespecs))

(defun rsh-directories-exist-p (host pathspec &rest pathspecs)
   "Returns T if all of the subdirectories PATHSPECS exist in
directory PATHSPEC on HOST, or NIL otherwise."
  (apply #'rsh-pathnames-exist-p host pathspec #'rsh-list-directories
         pathspecs))

(defun rsh-make-directory (host pathspec)
  "Creates a new directory PATHSPEC on HOST and returns PATHSPEC."
  (close-process (rsh-exec host *mkdir-executable* pathspec))
  pathspec)

(defun rsh-ensure-directories-exist (host pathspec)
  "Ensures that PATHSPEC exists on HOST, creating addional parent
directories as required."
  (close-process (rsh-exec host *mkdir-executable* "-p" pathspec))
  pathspec)

(defun rsh-pathname-exists-p (host pathspec1 pathspec2 rsh-list-fn)
  "Returns T if PATHSPEC1 exists in directory PATHSPEC2 on HOST, using
RSH-LIST-FN to find the candidates for PATHSPEC1."
  (let ((pname (parse-namestring pathspec2)))
    (some (lambda (namestring)
            (pathname-match-p (parse-namestring namestring) pname))
          (funcall rsh-list-fn host pathspec1))))

(defun rsh-pathnames-exist-p (host pathspec rsh-list-fn &rest pathspecs)
  "Returns T if all of PATHSPECS exists in directory PATHSPEC on HOST,
using RSH-LIST-FN to find the candidates for PATHSPEC."
  (let ((pnames (mapcar #'parse-namestring pathspecs))
        (remote-pnames (mapcar #'parse-namestring
                               (funcall rsh-list-fn host pathspec))))
    (every (lambda (pname)
             (member pname remote-pnames :test #'pathname-match-p))
           pnames)))

(defun rsh-list (host pathspec &optional (ignore-backups t))
  "Returns a list of pathspecs in PATHSPEC on HOST. If IGNORE-BACKUPS
is T (the default) then tilda backup files are ignored."
  (let ((rsh (apply #'rsh-exec host *ls-executable*
                    (if ignore-backups
                        (list "-B" "-p" pathspec)
                      (list "-p" pathspec)))))
    (unwind-protect
         (loop
            with stream = (output-of rsh)
            for line = (read-line stream nil nil)
            while line
            collect line)
      (close-process rsh))))
