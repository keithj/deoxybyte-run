;;;
;;; Copyright (C) 2007-2010 Keith James. All rights reserved.
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
  (make-array (length str) :element-type 'octet
              :initial-contents (loop for c across str
                                     collect (char-code c))))

(defun as-chars (bytes)
  (make-array (length bytes) :element-type 'base-char
              :initial-contents (loop for b across bytes
                                     collect (code-char b))))

(addtest (deoxybyte-run-tests) gnuplot/png/1
  (ensure-directories-exist (merge-pathnames "data"))
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
                                :element-type 'octet)
      (ensure (equalp *png-signature* (loop
                                         repeat 8
                                         collect (read-byte png-stream)))))
    (delete-file png-filespec)))

(addtest (deoxybyte-run-tests) gnuplot/x11/histogram/1
  (let* ((plotter (run-gnuplot :debug nil))
         (x (list "foo" "bar" "baz"))
         (y (list 2.1 2.2 5.6))
         (plot (make-instance 'histogram
                              :x-axis (x-axis :label "x" :range (list -0.5 6.5))
                              :y-axis (y-axis :label "y" :range (list -0.5 6.5))
                              :series (make-instance 'category-series
                                                     :categories x :values y)
                        :title "Histogram")))
    (draw-plot plotter plot :terminal :x11)
    (sleep 5)
    (stop-gnuplot plotter)))

(addtest (deoxybyte-run-tests) gnuplot/x11/update/1
  (let* ((plotter (run-gnuplot :debug nil))
         (x (list 0))
         (y0 (list 0 1.1 1.3 1.9 2.3 4.5))
         (y1 (list 0 0.7 1.2 2.1 2.2 5.6))
         (plot (2d-plot (x-axis :label "x" :range (list -0.5 6.5))
                        (y-axis :label "y" :range (list -0.5 6.5))
                        (list (xy-series x (list 0) :style '(:linespoints))
                              (xy-series x (list 0) :style '(:linespoints)))
                        :title "Update")))
    (draw-plot plotter plot :terminal :x11)
    (loop
       for i from 1 to 5
       do (let ((s0 (nth-series-of 0 plot))
                (s1 (nth-series-of 1 plot)))
            (sleep 1)
            (append-data s0 :x-values (list i) :y-values (list (nth i y0)))
            (append-data s1 :x-values (list i) :y-values (list (nth i y1)))
            (update-plot plotter plot)))
    (stop-gnuplot plotter)))
