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

(defparameter *rsh-executable* "rsh"
  "The rsh executable name.")
(defparameter *ls-executable* "ls"
  "The ls executable name.")
(defparameter *mkdir-executable* "mkdir"
  "The mkdir executable name.")

(defparameter *wait-for-rsh-process*
  #+:sbcl t
  #+:lispworks nil
  #-(or :sbcl :lispworks) t
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
{defclass rsh} . If the command returns a non-zero exit code, a
NON-ZERO-EXIT-ERROR error is raised."
  (let ((rsh (make-instance 'rsh :program *rsh-executable*
                                 :host host :args (cons host command)
                                 :input :stream :output :stream
                                 :search t :wait *wait-for-rsh-process*
                                 :allow-other-keys t)))
    (if (zerop (exit-code-of rsh))
        rsh
      (error 'non-zero-exit-error :program rsh))))

(defun merge-remote-pathnames (pathname
                               &optional (defaults *remote-pathname-defaults*))
  "Constructs a filled in pathname by completing the unspecified
components from the defaults given by *remote-pathname-defaults*."
  (merge-pathnames pathname defaults))

(defun rsh-list-directory (host pathspec &key (ignore-backups t) filetype)
  "Returns a list of namestrings of files in directory PATHSPEC on
HOST. PATHSPEC is first merged with remote pathname defaults. If
IGNORE-BACKUPS is T (the default) then tilda backup files are
ignored. The FILETYPE keyword may :file to return only
files, :directory to return only directories or NIL to return both
files and directories."
  (let ((dir (merge-remote-pathnames (fad:pathname-as-directory pathspec))))
    (loop
       for filename in (rsh-ls host (namestring dir) ignore-backups)
       if (ends-with-char-p filename #\/) collect filename into dirs
       else collect filename into files
       finally (return (ecase filetype
                         (:file files)
                         (:directory dirs)
                         ((nil) (nconc dirs files)))))))

(defun rsh-file-exists-p (host filespec)
  "Returns T if file FILESPEC exists on HOST, or NIL
otherwise. FILESPEC is first merged with remote pathname defaults."
  (let ((file (merge-remote-pathnames (fad:pathname-as-file filespec))))
    (multiple-value-bind (ps fs)
        (right-trim-path file)
      (rsh-pathname-exists-p host ps fs :filetype :file))))

(defun rsh-directory-exists-p (host pathspec)
  "Returns T if PATHSPEC exists on HOST, or NIL otherwise. PATHSPEC is
first merged with remote pathname defaults."
  (let ((dir (merge-remote-pathnames (fad:pathname-as-directory pathspec))))
    (multiple-value-bind (ps1 ps2)
        (right-trim-path dir)
      (rsh-pathname-exists-p host ps1 ps2 :filetype :directory))))

(defun rsh-files-exist-p (host &rest filespecs)
  "Returns T if all FILESPECS exist in directory on HOST, or NIL
otherwise. Each FILESPEC is first merged with remote pathname
defaults."
  (subsetp filespecs (rsh-list-directory host *remote-pathname-defaults*
                                         :filetype :file) :test #'equal))

(defun rsh-directories-exist-p (host &rest pathspecs)
  "Returns T if all PATHSPECS exist in directory on HOST, or NIL
otherwise. Each PATHSPEC is first merged with remote pathname
defaults."
  (subsetp (mapcar (lambda (ps)
                     (namestring (fad:pathname-as-directory ps))) pathspecs)
           (rsh-list-directory host *remote-pathname-defaults*
                               :filetype :directory) :test #'equal))

(defun rsh-make-directory (host pathspec)
  "Creates a new directory specified by PATHSPEC on HOST and returns
PATHSPEC. PATHSPEC is first merged with remote pathname defaults."
  (rsh-mkdir host pathspec))

(defun rsh-ensure-directories-exist (host pathspec)
  "Ensures that PATHSPEC exists on HOST, creating addional parent
directories as required.  PATHSPEC is first merged with remote
pathname defaults."
  (rsh-mkdir host pathspec "-p"))

(defun rsh-pathname-exists-p (host pathname1 pathname2 &key filetype)
  "Returns T if PATHNAME1 exists in directory PATHNAME2 on HOST, using
RSH-LIST-FN to find the candidates for PATHNAME1."
  (some (lambda (pn)
          (equal pn (namestring pathname2)))
        (rsh-list-directory host pathname1 :filetype filetype)))

(defun rsh-ls (host pathspec &optional (ignore-backups t))
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

(defun rsh-mkdir (host pathspec &rest mkdir-args)
  "Executes mkdir to create a directory specified by PATHSPEC on HOST
using mkdir arguments MKDIR-ARGS. Returns PATHSPEC."
  (let ((dir (fad:pathname-as-directory pathspec)))
    (cond ((absolute-pathname-p dir)
           (close-process (apply #'rsh-exec host *mkdir-executable*
                                 (append mkdir-args (list pathspec))))
           pathspec)
          (t
           (error 'invalid-argument-error
                  :params 'pathspec
                  :args pathspec
                  :text "the pathspec must be absolute, not relative")))))

(defun right-trim-path (pathname)
  "Returns two values, being the root of PATHNAME with the terminal
file or directory component removed and the removed component itself."
  (if (fad:directory-pathname-p pathname)
      (let ((dir (pathname-directory (fad:pathname-as-directory pathname))))
        (values (make-pathname :directory (butlast dir))
                (make-pathname :directory (cons :relative (last dir)))))
    (values (make-pathname :directory (pathname-directory pathname))
            (make-pathname :directory '(:relative) :defaults pathname))))
