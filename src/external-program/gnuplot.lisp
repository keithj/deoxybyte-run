;;;
;;; Copyright (C) 2008 Keith James. All rights reserved.
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

(in-package :gnuplot)

;;; I find format cryptic, but it's very concise. The directives I'm
;;; using here are:
;;;
;;; ~( ... ~) fold into lower case
;;; ~@[ ... ~] conditional format
;;; ~{~^ ... ~} list iteration
;;;

(defclass gnuplot (external-program)
  ()
  (:documentation "An instance of this class represents a Gnuplot
process."))

(defclass plot ()
  ((title :initform nil
          :initarg :title
          :accessor title-of
          :documentation "The plot title.")
   (series :initform nil
           :initarg :series
           :accessor series-of
           :documentation "A series object, or list of series objects
to be rendered on the plot.")
   (legend :initform t
           :initarg :legend
           :accessor legend-of
           :documentation "A specifier for the plot legend.")))

(defclass 2d-plot (plot)
  ((x-axis :initform nil
           :initarg :x-axis
           :accessor x-axis-of
           :documentation "An axis object representing the x-axis.")
   (y-axis :initform nil
           :initarg :y-axis
           :accessor y-axis-of
           :documentation "An axis object representing the y-axis.")))

(defclass axis ()
  ((position :initform nil
             :initarg :position
             :accessor position-of
             :documentation "A specifier for the position and
orientation of the axis.")
   (range :initform nil
          :initarg :range
          :accessor range-of
          :documentation "A specifier for the visible range of the
axis.")
   (label :initform nil
          :initarg :label
          :accessor label-of
          :documentation "The axis label.")
   (tics :initform nil
         :initarg :tics
         :accessor tics-of
         :documentation "A specifier for the axis major tic position
and style.")
   (minor-tics :initform nil
               :initarg :minor-tics
               :accessor minor-tics-of
               :documentation "A specifier for the axis minor tic
position and style.")))

(defclass series ()
  ((style :initform nil
          :initarg :style
          :accessor style-of
          :documentation "A specifier for plot style of the series."))
  (:documentation "A data series to be plotted."))

(defclass xy-series (series)
  ((x-values :initform nil
             :initarg :x-values
             :accessor x-values-of)
   (y-values :initform nil
             :initarg :y-values
             :accessor y-values-of))
  (:documentation "An xy series of two sequences of equal length."))

(defmethod initialize-instance :after ((series xy-series) &key)
  (with-slots (x-values y-values) series
    (unless (= (length x-values) (length y-values))
      (error 'invalid-argument-error
             :params '(:x-values :y-values)
             :args (list x-values y-values)
             :text "The x and y sequences are not the same length."))))

(defgeneric length-of (series)
  (:documentation "Returns the length of SERIES."))

(defgeneric format-axis (plotter axis)
  (:documentation "Renders AXIS using PLOTTER."))

(defgeneric format-series (plotter series)
  (:documentation "Renders SERIES using PLOTTER."))

(defgeneric draw-plot (plotter plot &key terminal output))

(defmethod length-of ((series xy-series))
  (length (x-values-of series)))

(defmethod format-axis ((plotter gnuplot) (axis axis))
  (let ((stream (input-stream-of plotter)))
    (with-slots (position label tics minor-tics) axis
      (when label
        (format stream "set ~(~a~)label ~s~%" position label))
      (when tics
        (format stream "set ~(~a~)tics ~a~%" position tics))
      (when minor-tics
        (format stream "set ~(~a~)mtics ~a~%" position minor-tics)))))

(defmethod format-series ((plotter gnuplot) (series xy-series))
  (let ((stream (input-stream-of plotter)))
    (format stream " ~@[with ~{~^~(~a~) ~}~]~%" (style-of series))
    (loop
       with x = (x-values-of series)
       and y = (y-values-of series)
       for i from 0 below (length-of series)
       do (format stream "~a ~a~%" (elt x i) (elt y i))
       finally (format stream "e~%"))))

(defmethod format-series ((plotter gnuplot) (series list))
  (dolist (s series)
    (format-series plotter s)))

(defmethod draw-plot ((plotter gnuplot) (plot 2d-plot)
                      &key (terminal :x11) output)
  (let ((stream (input-stream-of plotter)))
    (with-slots (title x-axis y-axis series) plot
      (princ "set terminal " stream)
      (if (listp terminal)
          (format stream "~{~^~(~a~) ~}~%" terminal)
        (format stream "~(~a~)~%" terminal))
      (format stream "set output~@[ ~s~]~%" output)
      (format stream "set title \"~a\"~%" title)
      (if (and (legend-of plot)
               (listp (legend-of plot)))
          (format stream "set key ~{~^~(~a~) ~}~%" (legend-of plot))
        (format stream "set key off~%"))
      (format-axis plotter x-axis)
      (format-axis plotter y-axis)
      (format stream "plot \"-\"")
      (format-series plotter (series-of plot)))))

(defmethod draw-plot :after ((plotter gnuplot) (plot 2d-plot)
                             &key terminal output)
  (declare (ignore terminal output))
  (force-output (input-stream-of plotter)))

(defun run-gnuplot (&rest args)
  "Starts a new Gnuplot process and returns a CLOS object of class
GNUPLOT."
  (apply #'make-instance 'gnuplot
         :program "gnuplot" :args '("-display" ":0.0") args))

(defun stop-gnuplot (plotter)
  "Stops the PLOTTER process."
  (let ((stream (input-stream-of plotter)))
    (write-line "quit" stream))
  t)

