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

(in-package :uk.co.deoxybyte-run-test)

(deftestsuite deoxybyte-run-tests ()
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

(addtest (deoxybyte-run-tests) gnuplot/1
  (let* ((png-filespec (namestring (merge-pathnames "data/xy-plot.png")))
         (plotter (run-gnuplot))
         (x #(0 1 2 3 4 5 6 7 8 9))
         (y #(1 2.5 3 4 5 6 6.5 4 3.2 3))
         (plot (make-instance
                '2d-plot
                :title "Test title"
                :x-axis (make-instance 'axis
                                       :label "Test x label" :position :x)
                :y-axis (make-instance 'axis
                                       :label "Test y label" :position :y)
                :series (make-instance 'xy-series
                                       :x-values x
                                       :y-values y
                                       :style '(:linespoints
                                                :smooth
                                                :csplines)))))
    (ensure (open-stream-p (input-of plotter)))
    (draw-plot plotter plot :terminal :png :output png-filespec)
    (stop-gnuplot plotter)
    (ensure (not (open-stream-p (input-of plotter))))
    (with-open-file (png-stream png-filespec :direction :input
                     :element-type '(unsigned-byte 8))
      (ensure (equalp *png-signature* (loop
                                         repeat 8
                                         collect (read-byte png-stream)))))
    (delete-file png-filespec)))
