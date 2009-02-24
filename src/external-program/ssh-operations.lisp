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

(declaim (type pathname *remote-pathname-defaults*))
(defparameter *remote-pathname-defaults* (pathname "/")
  "The defaults used to fill in remote pathnames.")

(defparameter *default-remote-host* "localhost")

(defgeneric list-directory (session pathspec &key ignore-backups filetype)
  (:documentation "Returns a list of pathnames of files in directory
PATHSPEC in SESSION. If IGNORE-BACKUPS is T (the default) then tilda
backup files are ignored. The FILETYPE keyword may :file to return
only files, :directory to return only directories or NIL to return
both files and directories."))

(defgeneric get-directory (session)
  (:documentation "Returns the pathname of the current directory of
  SESSION."))

(defgeneric set-directory (session pathspec)
  (:documentation "Sets the current directory of SESSION to
  PATHSPEC."))

(defgeneric file-exists-p (session pathspec &key filetype)
  (:documentation "Returns T if PATHSPEC exists in SESSION. The
FILETYPE keyword may :file to examine only files, :directory to
examine only directories or NIL to examine both files and
directories."))

(defgeneric make-directory (session pathspec)
  (:documentation "Creates a new directory PATHSPEC, including any
  necessary parent directories, in SESSION."))

(defgeneric delete-directory-and-files (session pathspec)
  (:documentation "Recursively deletes file or directory PATHSPEC in
  SESSION."))

(defmethod list-directory ((session ssh-session) pathspec
                           &key (ignore-backups t) filetype)
  (let ((dir (fad:pathname-as-directory pathspec)))
    (loop
       for filename in (remote-command session
                                       (format nil "[ -d ~a ] && ls~{ ~a~}"
                                               dir (if ignore-backups
                                                       (list "-B" "-p" dir)
                                                     (list "-p" dir))))
       if (ends-with-char-p filename #\/)
       collect (fad:pathname-as-directory filename) into dirs
       else
       collect (fad:pathname-as-file filename) into files
       finally (return (ecase filetype
                         (:file files)
                         (:directory dirs)
                         ((nil) (nconc dirs files)))))))

(defmethod get-directory ((session ssh-session))
  (fad:pathname-as-directory (first (remote-command session "pwd"))))

(defmethod set-directory ((session ssh-session) pathspec)
  (let ((dir (fad:pathname-as-directory pathspec)))
    (pathname
     (first (remote-command session (format nil "cd ~a ; pwd" dir))))))

(defmethod file-exists-p ((session ssh-session) pathspec &key (filetype :file))
  (handler-case
      (ecase filetype
        (:file (let ((file (fad:pathname-as-file pathspec)))
                 (remote-command session (format nil "[ -f ~a ]" file)
                                 :void-command t)
                 t))
        (:directory (let ((dir (fad:pathname-as-directory pathspec)))
                      (remote-command session (format nil "[ -d ~a ]" dir)
                                      :void-command t)
                      t)))
    (non-zero-exit-error (condition)
      (if (= 1 (exit-code-of condition)) ; exit code when file test fails
          nil
        (error 'condition)))))

(defmethod make-directory ((session ssh-session) pathspec)
  (let ((dir (fad:pathname-as-directory pathspec)))
    (remote-command session (format nil "[ ! -d ~a ] && mkdir -p ~a" dir dir)
                    :void-command t)
    dir))

(defmethod delete-directory-and-files ((session ssh-session) pathspec)
  (let ((dir (fad:pathname-as-directory pathspec)))
    (remote-command session (format nil "rm -r ~a" dir) :void-command t)
    dir))
