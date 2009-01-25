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

(in-package :cl-io-utilities-test)

(deftestsuite cl-io-utilities-tests ()
  ())

(defvar *png-signature* '(137 80 78 71 13 10 26 10)
  "The first eight bytes of any PNG file.")

(defun as-bytes (str)
  (make-array (length str) :element-type '(unsigned-byte 8)
              :initial-contents (loop for c across str
                                     collect (char-code c))))

(defun as-chars (bytes)
  (make-array (length bytes) :element-type 'base-char
              :initial-contents (loop for b across bytes
                                     collect (code-char b))))

;;; Gray-streams methods to be tested

;; input streams
;; sb-gray:stream-clear-input stream
;; sb-gray:stream-read-sequence stream seq &optional start binary

;; end streams
;; sb-gray:stream-read-byte stream
;; sb-gray:stream-write-byte stream integer

;; character input streams
;; sb-gray:stream-peek-char stream
;; sb-gray:stream-read-char-no-hang stream
;; sb-gray:stream-read-char stream
;; sb-gray:stream-read-line stream
;; sb-gray:stream-listen stream
;; sb-gray:stream-unread-char stream character

(addtest (cl-io-utilities-tests) gray-common/1
  (with-open-file (stream (merge-pathnames "data/test1.txt")
                   :direction :input
                   :element-type 'base-char
                   :external-format :ascii)
    (let ((s (make-line-input-stream stream)))
      (ensure (subtypep (stream-element-type s) 'character))
      (ensure (zerop (stream-file-position s)))
      (ensure (open-stream-p s))
      (ensure (close s))
      (ensure (not (open-stream-p s)))
      (ensure-error
       (stream-read-line s)))))

(addtest (cl-io-utilities-tests) gray-common/2
  (with-open-file (stream (merge-pathnames "data/test1.txt")
                   :direction :input
                   :element-type '(unsigned-byte 8))
    (let ((s (make-line-input-stream stream)))
      (ensure (equal (stream-element-type s) '(unsigned-byte 8)))
      (ensure (zerop (stream-file-position s)))
      (ensure (open-stream-p s))
      (ensure (close s))
      (ensure (not (open-stream-p s)))
      (ensure-error
       (stream-read-line s)))))

(addtest (cl-io-utilities-tests) gray-input/1
  (with-open-file (stream (merge-pathnames "data/test1.txt")
                   :direction :input
                   :element-type 'base-char
                   :external-format :ascii)
    (let ((s (make-line-input-stream stream))
          (b (make-array 10 :element-type 'character)))
      ;; stream-clear-input should empty the line buffer
      (push-line s "aaaa")
      (ensure (iou::line-stack-of s))
      (ensure-null (stream-clear-input s))
      (ensure (not (iou::line-stack-of s)))
      (ensure (= 10 (stream-read-sequence s b 0 (length b))))
      (ensure (string= "abcdefghij" b)))))

(addtest (cl-io-utilities-tests) gray-input/2
  (with-open-file (stream (merge-pathnames "data/test1.txt")
                   :direction :input
                   :element-type '(unsigned-byte 8))
    (let ((s (make-line-input-stream stream))
          (b (make-array 10 :element-type '(unsigned-byte 8))))
      (ensure-null (stream-clear-input s))
      (ensure (= 10 (stream-read-sequence s b 0 (length b))))
      (ensure (equalp (as-bytes "abcdefghij") b)))))

(addtest (cl-io-utilities-tests) gray-binary/1
  (with-open-file (stream (merge-pathnames "data/test1.txt")
                   :direction :input
                   :element-type '(unsigned-byte 8))
    (let ((s (make-line-input-stream stream))
          (line (as-bytes "abcdefghijklmnopqrstuvwxyz")))
      (loop for byte across line
           do (ensure (= byte (stream-read-byte s)))))))

(addtest (cl-io-utilities-tests) gray-char/1
  (with-open-file (stream (merge-pathnames "data/test1.txt")
                   :direction :input
                   :element-type 'base-char
                   :external-format :ascii)
    (let ((s (make-line-input-stream stream))
          (line "abcdefghijklmnopqrstuvwxyz"))
      (ensure (char= #\a (stream-read-char s)))
      (ensure-null (stream-unread-char s #\a))
      (loop for char across line
         do (ensure (char= char (stream-read-char s)))))))

(addtest (cl-io-utilities-tests) stream-read-line/1
  (with-open-file (stream (merge-pathnames "data/test1.txt")
                   :direction :input
                   :element-type 'base-char
                   :external-format :ascii)
    (let ((s (make-line-input-stream stream))
          (line "abcdefghijklmnopqrstuvwxyz"))
      (multiple-value-bind (line2 missing-newline-p)
          (stream-read-line s)
        (ensure (equalp line2 line))
        (ensure (not missing-newline-p)))
      (multiple-value-bind (line2 missing-newline-p)
          (stream-read-line s)
        (ensure (eql :eof line2))
        (ensure missing-newline-p)))))

(addtest (cl-io-utilities-tests) stream-read-line/2
  (with-open-file (stream (merge-pathnames "data/test1.txt")
                   :direction :input
                   :element-type '(unsigned-byte 8))
    (let ((s (make-line-input-stream stream))
          (line (as-bytes "abcdefghijklmnopqrstuvwxyz")))
      (multiple-value-bind (bytes missing-newline-p)
          (stream-read-line s)
        (ensure (equalp line bytes))
        (ensure (not missing-newline-p)))
      (multiple-value-bind (bytes missing-newline-p)
          (stream-read-line s)
        (ensure-same :eof bytes)
        (ensure missing-newline-p)))))

(addtest (cl-io-utilities-tests) push-line/1
  (with-open-file (stream (merge-pathnames "data/test1.txt")
                   :direction :input
                   :element-type 'base-char
                   :external-format :ascii)
    (let ((s (make-line-input-stream stream))
          (line "abcdefghijklmnopqrstuvwxyz"))
      (multiple-value-bind (line2 missing-newline-p)
          (stream-read-line s)
        (ensure (equalp line line2))
        (ensure (not missing-newline-p))
        (push-line s line2)
        (ensure (equalp line (stream-read-line s)))))))

(addtest (cl-io-utilities-tests) push-line/2
  (with-open-file (stream (merge-pathnames "data/test1.txt")
                   :direction :input
                   :element-type '(unsigned-byte 8))
    (let ((s (make-line-input-stream stream))
          (line (as-bytes "abcdefghijklmnopqrstuvwxyz")))
      (multiple-value-bind (bytes missing-newline-p)
          (stream-read-line s)
        (ensure (equalp line bytes))
        (ensure (not missing-newline-p))
        (push-line s bytes)
        (ensure (equalp line (stream-read-line s)))))))

(addtest (cl-io-utilities-tests) missing-newline-p/1
  (with-open-file (stream (merge-pathnames "data/test2.txt")
                   :direction :input
                   :element-type 'base-char
                   :external-format :ascii)
    (let ((s (make-line-input-stream stream))
          (lines '("1234567890"
                   "0987654321"
                   "abcdefghij"
                   "klmnopqrst")))
      (dolist (line (butlast lines))
        (multiple-value-bind (line2 missing-newline-p)
            (stream-read-line s)
          (ensure (equalp line line2))
          (ensure (not missing-newline-p))))
      (multiple-value-bind (line2 missing-newline-p)
          (stream-read-line s)
        (ensure (equalp (car (last lines)) line2))
        (ensure missing-newline-p)))))

(addtest (cl-io-utilities-tests) missing-newline-p/2
  (with-open-file (stream (merge-pathnames "data/test2.txt")
                   :direction :input
                   :element-type '(unsigned-byte 8))
    (let ((s (make-line-input-stream stream))
          (lines (mapcar #'as-bytes '("1234567890"
                                      "0987654321"
                                      "abcdefghij"
                                      "klmnopqrst"))))
      (dolist (line (butlast lines))
        (multiple-value-bind (bytes missing-newline-p)
            (stream-read-line s)
          (ensure (equalp line bytes))
          (ensure (not missing-newline-p))))
      (multiple-value-bind (bytes missing-newline-p)
          (stream-read-line s)
        (ensure (equalp (car (last lines)) bytes))
        (ensure missing-newline-p)))))

(addtest (cl-io-utilities-tests) find-line/1
  (with-open-file (stream (merge-pathnames "data/test3.txt")
                   :direction :input
                   :element-type '(unsigned-byte 8))
    (let ((s (make-line-input-stream stream))
          (lines (mapcar #'as-bytes '("abc"
                                      "def"
                                      "ghi"
                                      "jkl"
                                      "mno"
                                      "pqr"
                                      "stu"
                                      "vwx"
                                      "yz"))))
      ;; Success at line 1 of max 1 -> "abc"
      (multiple-value-bind (line found line-count)
          (find-line s #'(lambda (a)
                            (= (aref a 0) (char-code #\a))) 1)
        (ensure (equalp (nth 0 lines) line))
        (ensure found)
        (ensure (= 1 line-count)))
      ;; Fail at line 1 of max 1 -> "def"
      (multiple-value-bind (line found line-count)
          (find-line s #'(lambda (a)
                            (= (aref a 0) (char-code #\Z))) 1)
        (ensure (equalp (nth 1 lines) line))
        (ensure (not found))
        (ensure (= 1 line-count)))
      ;; Success at line 3 of max 4
      (multiple-value-bind (line found line-count)
          (find-line s #'(lambda (a)
                            (= (aref a 0) (char-code #\m))) 4)
        (ensure (equalp (nth 4 lines) line))
        (ensure found)
        (ensure (= 3 line-count)))
      ;; Fall through to eof
      (multiple-value-bind (line found line-count)
          (find-line s #'(lambda (a)
                            (declare (ignore a))
                            nil))
        (ensure-same :eof line)
        (ensure (not found))
        (ensure (= 5 line-count))))))

(addtest (cl-io-utilities-tests) make-tmp-pathname/1
  ;; Test defaults
  (ensure (pathnamep (make-tmp-pathname)))
  (ensure (string= "/tmp/" (directory-namestring (make-tmp-pathname))))
  (ensure (integerp (parse-integer (pathname-name (make-tmp-pathname)))))
  (ensure (string= "tmp" (pathname-type (make-tmp-pathname))))
  ;; Test optional arguments
  (ensure (string= "/" (directory-namestring (make-tmp-pathname
                                              :tmpdir "/"))))
  (ensure (string= "foo" (pathname-name (make-tmp-pathname :tmpdir "/tmp"
                                                           :basename "foo"))
                   :end2 3))
  (ensure (string= "bar" (pathname-type
                          (make-tmp-pathname :tmpdir "/tmp"
                                             :basename "foo"
                                             :type "bar"))))
  ;; Test error condition
  (let ((bad-dir "/this-directory-does-not-exist/"))
    (ensure (and (not (fad:directory-exists-p bad-dir))
                 (ensure-condition gpu:invalid-argument-error
                                   (make-tmp-pathname :tmpdir bad-dir))))))

(addtest (cl-io-utilities-tests) gnuplot/1
  (let* ((png-filespec (namestring (merge-pathnames "data/xy-plot.png")))
         (plotter (gpt:run-gnuplot))
         (x #(0 1 2 3 4 5 6 7 8 9))
         (y #(1 2.5 3 4 5 6 6.5 4 3.2 3))
         (plot (make-instance
                'gpt:2d-plot
                :title "Test title"
                :x-axis (make-instance 'gpt:axis
                                       :label "Test x label" :position :x)
                :y-axis (make-instance 'gpt:axis
                                       :label "Test y label" :position :y)
                :series (make-instance 'gpt:xy-series
                                       :x-values x
                                       :y-values y
                                       :style '(:linespoints
                                                :smooth
                                                :csplines)))))
    (ensure (open-stream-p (input-of plotter)))
    (gpt:draw-plot plotter plot :terminal :png :output png-filespec)
    (gpt:stop-gnuplot plotter)
    (ensure (not (open-stream-p (input-of plotter))))
    (with-open-file (png-stream png-filespec :direction :input
                     :element-type '(unsigned-byte 8))
      (ensure (equalp *png-signature* (loop
                                         repeat 8
                                         collect (read-byte png-stream)))))
    (delete-file png-filespec)))
