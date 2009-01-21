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

(defun test-sort (in-filespec out-filespec buffer-size)
  (with-open-file (in in-filespec :direction :input
                   :element-type 'base-char
                   :external-format :ascii)
    (with-open-file (out out-filespec :direction :output
                     :element-type 'base-char
                     :external-format :ascii
                     :if-exists :supersede)
      (external-merge-sort in out #'string<= :buffer-size buffer-size))))

(defun make-buffer (stream)
  "Returns an object wrapping STREAM with a one-line read buffer."
  (cons (read-line stream) stream))

(defun buffer-peek (buffer-stream)
  "Returns the first available line in BUFFER-STREAM, without
reading."
  (car buffer-stream))

(defun buffer-refill (buffer-stream)
  "Reads and caches the next line from BUFFER-STREAM, returning
BUFFER-STREAM."
  (rplaca buffer-stream (read-line (cdr buffer-stream) nil nil)))

(defun external-merge-sort (in out predicate
                            &key key (buffer-size 100000))
  "Performs an external merge sort of data from an input stream,
writing the sorted data to an output stream.

Arguments:

- in (input stream): A stream from which the data to be sorted are
read.
- out (output stream): A stream to which the sorted data are written.
- predicate (function): A designator for a function of two arguments
that returns a generalised boolean.

Optional:

-key (function): A designator for a function of one argument, or nil.
- buffer-size (fixnum): The number of lines to be sorted in memory at
one time."
  (let ((chunk-streams (external-sort in predicate buffer-size :key key)))
    (unwind-protect
         (loop
            with buffers =
              (make-array (length chunk-streams)
                          :initial-contents
                          (loop
                             for s in chunk-streams
                             collect (make-buffer s)))
            as line = (merge-next-line buffers predicate :key key)
            while line
            do (write-line line out))
      (dolist (s chunk-streams)
        (when (open-stream-p s)
          (close s))
        (delete-file s)))))

(defun merge-next-line (buffers predicate &key key)
  "Returns the next line in the ordering described by PREDICATE from
one of BUFFERS."
  (loop
     with found = (buffer-peek (aref buffers 0))
     and found-idx = 0
     for test-idx from 0 below (length buffers)
     for line = (buffer-peek (aref buffers test-idx))
     when (and line (or (null found)
                        (funcall predicate (if key
                                               (funcall key line)
                                             line) found)))
     do (setf found line
              found-idx test-idx)
     finally (progn
               (setf (aref buffers found-idx)
                     (buffer-refill (aref buffers found-idx)))
               (return found))))

(defun external-sort (stream predicate buffer-size &key key)
  "Repeatedly reads up to BUFFER-SIZE lines from STREAM, sorts them
according to PREDICATE and writes them to a temporary file, until all
available data in STREAM has been exhausted. Returns a list of open
bi-diectional streams to the temporary files, ready for reading during
the merge step."
  (let ((type (stream-element-type stream))
        (format (stream-external-format stream))
        (chunk-streams '()))
    (flet ((save-chunk (chunk)
             (let ((out (open (make-tmp-pathname :basename "sort")
                              :direction :io
                              :element-type type
                              :external-format format)))
               (write-sorted-chunk chunk predicate out :key key)
               (file-position out 0)
               (push out chunk-streams))
             chunk-streams))
      (handler-case
          (loop
             with chunk = (make-array buffer-size :adjustable t
                                      :fill-pointer 0)
             and i = 0
             as line = (read-line stream nil nil)
             while line
             do (progn
                  (vector-push line chunk)
                  (incf i)
                  (unless (< i buffer-size)
                    (save-chunk chunk)
                    (adjust-array chunk buffer-size :fill-pointer 0)
                    (setf i 0)))
             finally (unless (zerop (length chunk))
                       (save-chunk chunk)))
        (error (file-error)
          (dolist (s  chunk-streams)
            (when (open-stream-p s)
              (close s :abort t)))
          (error file-error))))
    chunk-streams))

(defun write-sorted-chunk (chunk predicate stream &key key)
  "Sorts vector of lines CHUNK by PREDICATE and writes them to
STREAM."
  (let ((*print-pretty* nil)
        (sorted (sort chunk predicate :key key)))
    (loop
       for line across sorted
       do (progn
            (princ line stream)
            (terpri stream)))))

(defun count-occurrences (in out test)
  (loop
     with current = nil
     and count = 0
     as line = (read-line in nil nil)
     while line
     do (cond ((null current)
               (setf current line
                     count 1))
              ((funcall test line current)
               (incf count))
              (t
               (princ current out)
               (write-char #\Tab out)
               (princ count out)
               (terpri out)
               (setf current line
                     count 1)))
       finally (when current
                 (princ current out)
                 (write-char #\Tab out)
                 (princ count out)
                 (terpri out))))

(defun test-count (in-filespec out-filespec)
  (with-open-file (in in-filespec :direction :input
                   :element-type 'base-char
                   :external-format :ascii)
    (with-open-file (out out-filespec :direction :output
                     :element-type 'base-char
                     :external-format :ascii
                     :if-exists :supersede)
      (count-occurrences in out #'string<=))))
