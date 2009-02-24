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

(in-package :cl-io-utilities-test)

(addtest (cl-io-utilities-tests) remote-shell-executable/1
  (ensure (every (lambda (symbol pathname)
                   (equal (iou::remote-shell-executable symbol) pathname))
                 '(:sh :bash :csh :tcsh)
                 '(#P"/bin/sh" #P"/bin/bash" #P"/bin/tcsh" #P"/bin/tcsh"))))

(addtest (cl-io-utilities-tests) ssh-session/1
  (let ((session (open-session :remote-environment (pairlis '(X Y) '(A B)))))
    (ensure (open-session-p session)
            :report "Expected an open session.")
    (ensure (equalp '("A") (get-env session 'X))
            :report "Expected remote environment var X to have value A.")
    (ensure (equalp '("B") (get-env session 'Y))
            :report "Expected remote environment var X to have value A.")
    (close-session session)
    (sleep 4) ;; wait for the session to close - how to do this portably?
    (ensure (not (open-session-p session))
            :report "Expected a closed session.")))

(addtest (cl-io-utilities-tests) ssh-session/2
  (let ((session (open-session)))
    ;; Success
    (multiple-value-bind (response code)
        (remote-command session "ls /" :non-zero-error t :exit-code t
                        :void-command nil)
      (ensure response)
      (ensure (zerop code)))
     ;; Success with void command
    (multiple-value-bind (response code)
        (remote-command session "cd" :non-zero-error t :exit-code t
                        :void-command t)
      (ensure-null response)
      (ensure (zerop code)))
    ;; Fail without error
    (multiple-value-bind (response code)
        (remote-command session "ls /non_existant_directory"
                        :non-zero-error nil :exit-code t :void-command nil)
      (declare (ignore response))
      (ensure (plusp code)))
    ;; Fail with error
    (ensure-condition non-zero-exit-error
      (remote-command session "ls /non_existant_directory"
                      :non-zero-error t :exit-code t :void-command nil))
    (close-session session)))

(addtest (cl-io-utilities-tests) list-directory/1
  (let ((session (open-session)))
    (ensure (equalp '(#P"test1.txt" #P"test2.txt" #P"test3.txt")
                    (list-directory session (merge-pathnames "data/")
                                    :filetype :file)))
    (ensure (equalp '(#P"testdir1/" #P"testdir2/" #P"testdir3/")
                    (list-directory session (merge-pathnames "data/")
                                    :filetype :directory)))
    (ensure (equalp '(#P"testdir1/" #P"testdir2/" #P"testdir3/"
                      #P"test1.txt" #P"test2.txt" #P"test3.txt")
                    (list-directory session (merge-pathnames "data/"))))
    (close-session session)))

(addtest (cl-io-utilities-tests) get-directory/1
  (let ((session (open-session)))
    (ensure (equal (user-homedir-pathname) (get-directory session)))
    (close-session session)))

(addtest (cl-io-utilities-tests) set-directory/1
  (let ((session (open-session)))
    (ensure (equal (user-homedir-pathname) (get-directory session)))
    (ensure (equal #P"/tmp" (set-directory session #P"/tmp")))
    (close-session session)))

(addtest (cl-io-utilities-tests) file-exists-p/1
  (let ((session (open-session)))
    (ensure (file-exists-p session (merge-pathnames "data/test1.txt")))
    (ensure (file-exists-p session (merge-pathnames "data/test1.txt")
                           :filetype :file))
    (ensure (not (file-exists-p session (merge-pathnames "data/test99.txt"))))
    (close-session session)))

(addtest (cl-io-utilities-tests) file-exists-p/2
  (let ((session (open-session)))
    (ensure (file-exists-p session (merge-pathnames "data/testdir1")
                           :filetype :directory))
    (close-session session)))

(addtest (cl-io-utilities-tests) make-directory/1
  (let ((session (open-session))
        (dir (merge-pathnames "data/testdir99")))
    (ensure (make-directory session dir))
    (ensure (file-exists-p session dir :filetype :directory))
    (close-session session)
    (fad:delete-directory-and-files dir)))

(addtest (cl-io-utilities-tests) delete-directory-and-files/1
  (let ((session (open-session))
        (dir (merge-pathnames "data/testdir99/")))
    (ensure-directories-exist dir)
    (with-open-file (s (merge-pathnames "test99.txt" dir)
                       :direction :output :if-exists :error)
      (declare (ignore s)))
    (ensure (delete-directory-and-files session dir))
    (ensure (not (fad:file-exists-p dir)))
    (close-session session)))
