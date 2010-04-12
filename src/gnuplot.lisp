;;;
;;; Copyright (C) 2008-2010 Keith James. All rights reserved.
;;;
;;; This file is part of deoxybyte-run.
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

(in-package :uk.co.deoxybyte-run)

;;; I find format cryptic, but it's very concise. The directives I'm
;;; using here are:
;;;
;;; ~( ... ~) fold into lower case
;;; ~@[ ... ~] conditional format
;;; ~{~^ ... ~} list iteration
;;;

(defclass gnuplot (external-program)
  ((plot-stream :initform nil
                :accessor plot-stream-of
                :documentation "The stream to which plotting
instructions are written. By default this is the input stream of the
Gnuplot process. In debug mode, this is a broadcast stream which
writes to the input stream of the Gnuplot process and also to
*error-output*."))
  (:documentation "An instance of this class represents a Gnuplot
process that is used to plot graphs in batch mode.

Plotting requires the creation of a plotter, in this case a running
Gnuplot {defclass external-program} , and a {defclass plot}
object. These are passed to {defmethod draw-plot} which renders to an
output specified by additional keyword parameters.

Plots themselves contain subordinate {defclass axis} objects, and one
or more {defclass series} objects that contain the data to be
represented.

The mode of operation is that plot objects are independent of the
plotter. Changing a slot value in a plot, axis or series will not
cause an active plot to be updated.

An example of an XY plot of one series, plotted to a PNG file and also
to the screen, with dynamic updates.

;;; (let ((plotter (run-gnuplot))
;;;       (plot (make-instance
;;;              '2d-plot
;;;              :title "Foo"
;;;              :legend '(:tmargin :left)
;;;              :x-axis (make-instance 'axis :label "foo x"
;;;                                     :position :x)
;;;              :y-axis (make-instance 'axis :label "foo y"
;;;                                     :position :y)
;;;              :series (make-instance 'xy-series
;;;                                     :x-values
;;;                                     (list -1 0 3 4 5 8)
;;;                                     :y-values
;;;                                     (list -10 2 4 5 17 10)
;;;                                     :style '(:linespoints
;;;                                              :smooth
;;;                                              :csplines)))))
;;;   (draw-plot plotter plot :terminal :png :output "foo.png")
;;;   (draw-plot plotter plot :terminal :x11)
;;;   (let ((s (series-of plot)))
;;;                 (dotimes (n 5)
;;;                   (with-accessors ((y y-values-of))
;;;                       s
;;;                     (setf y (mapcar #'1+ y)))
;;;                   (update-plot plotter plot)
;;;                   (sleep 1)))
;;;   (stop-gnuplot plotter))"))

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
           :documentation "A specifier for the plot legend.")
   (x-axis :initform nil
           :initarg :x-axis
           :accessor x-axis-of
           :documentation "An axis object representing the x-axis.")
   (y-axis :initform nil
           :initarg :y-axis
           :accessor y-axis-of
           :documentation "An axis object representing the y-axis.")))

(defclass 2d-plot (plot)
  ())

(defclass histogram (2d-plot)
  ())

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
axis. Ranges are represented as a list of two numbers, being the upper
and lower bounds.")
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
             :accessor x-values-of
             :documentation "A sequence of numbers.")
   (y-values :initform nil
             :initarg :y-values
             :accessor y-values-of
             :documentation "A sequence of numbers."))
  (:documentation "An xy series of two sequences of equal length."))

(defclass category-series (series)
  ((categories :initform nil
               :initarg :categories
               :accessor categories-of)
   (values :initform nil
           :initarg :values
           :accessor values-of)))

(defun 2d-plot (x-axis y-axis series &rest initargs)
  "Convenience constructor for 2D-PLOT objects."
  (apply #'make-instance '2d-plot :x-axis x-axis :y-axis y-axis :series series
         initargs))

(defun x-axis (&rest initargs)
  "Convenience constructor for x-axes."
  (apply #'make-instance 'axis :position :x initargs))

(defun y-axis (&rest initargs)
  "Convenience constructor for y-axes."
  (apply #'make-instance 'axis :position :y initargs))

(defun xy-series (x-values y-values &rest initargs)
  "Convenience constructor for xy-series."
  (apply #'make-instance 'xy-series :x-values x-values :y-values y-values initargs))

(defun category-series (categories values &rest initargs)
  "Convenience constructor for category-series."
  (apply #'make-instance 'category-series :categories categories :values values))

(defmethod initialize-instance :after ((plotter gnuplot) &key debug
                                       &allow-other-keys)
  (unless (runningp plotter)
    (error "Gnuplot failed to start ~a" plotter))
  (with-accessors ((input input-of) (stream plot-stream-of))
      plotter
    (setf stream (if debug
                     (make-broadcast-stream input *error-output*)
                   input))))

(defmethod initialize-instance :after ((series xy-series) &key)
  (with-slots (x-values y-values)
      series
    (check-arguments (= (length x-values) (length y-values)) (x-values y-values)
                     "The x and y sequences are not the same length.")))

(defmethod initialize-instance :after ((series category-series) &key)
  (with-slots (categories values)
      series
    (check-arguments (= (length categories) (length values)) (categories values)
                     "The category and value sequences are not the same length.")))

(defmethod print-object ((plot plot) stream)
  (print-unreadable-object (plot stream :type t)
    (with-slots (title x-axis y-axis)
        plot
      (format stream "~s ~a ~a" title x-axis y-axis))))

(defmethod print-object ((axis axis) stream)
  (print-unreadable-object (axis stream :type t)
    (with-slots (position label range)
        axis
      (format stream "~a ~s ~a" position label range))))

(defmethod print-object ((series series) stream)
  (print-unreadable-object (series stream :type t)
    (princ (style-of series) stream)))

(defmethod print-object ((series xy-series) stream)
  (print-unreadable-object (series stream :type t)
    (with-slots (x-values y-values style)
        series
      (format stream "~d x ~d ~a" (length x-values) (length y-values) style))))

(defgeneric nth-series-of (n plot)
  (:documentation "Returns the Nth series of PLOT."))

(defgeneric length-of (series)
  (:documentation "Returns the length of SERIES."))

(defgeneric x-range-of (xy-series)
  (:documentation "Returns the range of the x-axis of XY-SERIES and a
list of two integers,"))

(defgeneric y-range-of (xy-series)
  (:documentation "Returns the range of the y-axis of XY-SERIES and a
list of two integers,"))

(defgeneric format-axis (plotter axis)
  (:documentation "Renders AXIS using PLOTTER."))

(defgeneric format-series (plotter series)
  (:documentation "Renders SERIES using PLOTTER."))

(defgeneric header-of (series)
  (:documentation "Returns a Gnuplot header string for SERIES."))

(defgeneric write-header (series stream)
  (:documentation "Writes the Gnuplot header data for SERIES to STREAM."))

(defgeneric write-data (series stream)
  (:documentation "Writes the Gnuplot data of SERIES to STREAM."))

(defgeneric draw-plot (plotter plot &key terminal output)
  (:documentation "Renders a single PLOT, that is, title, axes, legend
and one or more series."))

(defgeneric update-plot (plotter plot)
  (:documentation "Updates a single PLOT, replotting axes and
series. Any changes to axis ranges or series data will be reflected in
the plot. By replacing or modifying the axes or series of PLOT and
calling this method in a loop, dynamic plots may be achieved."))

(defmethod nth-series-of ((n integer) (plot plot))
  (with-slots (series)
      plot
    (check-arguments (or (and (listp series) (< n (list-length series)))
                         (zerop n)) (n) "index out of bounds")
    (if (listp series)
        (nth n series)
      series)))

(defmethod length-of ((series xy-series))
  (length (slot-value series 'x-values)))

(defmethod length-of ((series category-series))
  (length (slot-value series 'categories)))

(defmethod x-range-of ((series xy-series))
  (series-slot-range series 'x-values))

(defmethod x-range-of ((series list))
  (series-aggregate-range series #'x-range-of))

(defmethod y-range-of ((series xy-series))
  (series-slot-range series 'y-values))

(defmethod y-range-of ((series list))
  (series-aggregate-range series #'y-range-of))

(defmethod append-data ((series xy-series) &key x-values y-values)
  (with-slots ((x x-values) (y y-values))
      series
    (setf x (concatenate (type-of x) x x-values)
          y (concatenate (type-of y) y y-values))))

(defmethod format-axis ((plotter gnuplot) (axis axis))
  (let ((stream (plot-stream-of plotter)))
    (with-slots (position label tics minor-tics range)
        axis
      (when label
        (format stream "set ~(~a~)label ~s~%" position label))
      (when tics
        (format stream "set ~(~a~)tics ~a~%" position tics))
      (when minor-tics
        (format stream "set ~(~a~)mtics ~a~%" position minor-tics))
      (when range
        (format stream "set ~(~a~)range [~a:~a]~%" position
                (first range) (second range))))
    axis))

(defmethod header-of ((series series))
  (format nil " '-'~@[ with ~{~^~(~a~) ~}~]" (style-of series)))

(defmethod header-of ((series xy-series))
  (format nil " '-'~@[ with ~{~^~(~a~) ~}~]" (style-of series)))

(defmethod header-of ((series category-series))
  (format nil " '-' using 2:xtic(1)~@[ with ~{~^~(~a~) ~}~]" (style-of series)))

(defmethod format-series ((plotter gnuplot) (series series))
  (let ((stream (plot-stream-of plotter)))
    (write-string (header-of series) stream)
    (terpri stream)
    (write-data series stream))
  series)

(defmethod format-series ((plotter gnuplot) (series list))
  (let ((stream (plot-stream-of plotter)))
    (dolist (s (butlast series))
      (write-string (header-of s) stream)
      (write-string "," stream))
    (write-string (header-of (car (last series))) stream)
    (terpri stream)
    (dolist (s series)
      (write-data s stream)))
  series)

(defmethod write-data ((series xy-series) (stream stream))
  (with-slots (x-values y-values)
      series
    (loop
       for i from 0 below (length-of series)
       do (format stream "~a ~a~%" (elt x-values i) (elt y-values i))
       finally (write-line "e" stream)))
  series)

(defmethod write-data ((series category-series) (stream stream))
  (with-slots (categories values)
      series
    (loop
       for i from 0 below (length-of series)
       do (format stream "~a ~a~%" (elt categories i) (elt values i))
       finally (write-line "e" stream)))
  series)

(defmethod draw-plot :before ((plotter gnuplot) (plot 2d-plot)
                              &key (terminal :x11) output)
  (let ((stream (plot-stream-of plotter)))
    (with-slots (title x-axis y-axis series)
        plot
      (princ "set terminal " stream)
      (if (listp terminal)
          (format stream "~{~^~(~a~) ~}~%" terminal)
        (format stream "~(~a~)~%" terminal))
      (format stream "set output~@[ ~s~]~%" output)
      (format stream "set title \"~a\"~%" title)
      (if (and (legend-of plot)
               (listp (legend-of plot)))
          (format stream "set key ~{~^~(~a~) ~}~%" (legend-of plot))
        (write-line "set key off" stream))
      (format-axis plotter x-axis)
      (format-axis plotter y-axis)))
  plot)

(defmethod draw-plot ((plotter gnuplot) (plot 2d-plot)
                      &key (terminal :x11) output)
  (declare (ignore terminal output))
  (let ((stream (plot-stream-of plotter)))
    (write-string "plot" stream)
    (format-series plotter (series-of plot)))
  plot)

(defmethod draw-plot ((plotter gnuplot) (plot histogram)
                      &key (terminal :x11) output)
  (declare (ignore terminal output))
  (let ((stream (plot-stream-of plotter)))
    (write-line "set style data histogram" stream)
    (write-string "plot" stream)
    (format-series plotter (series-of plot)))
  plot)

(defmethod draw-plot :after ((plotter gnuplot) (plot plot)
                             &key terminal output)
  (declare (ignore terminal output))
  (force-output (plot-stream-of plotter))
  plot)

(defmethod update-plot ((plotter gnuplot) (plot 2d-plot))
  (let ((stream (plot-stream-of plotter)))
    (with-slots (x-axis y-axis series)
        plot
      (format-axis plotter x-axis)
      (format-axis plotter y-axis)
      (write-string "plot" stream)
      (format-series plotter series)
      (destructuring-bind (ymin ymax)
          (y-range-of series)
        (let ((y-range (- ymax ymin)))
          (format stream "set yrange [~a:~a]~%" (- ymin (/ y-range 20))
                  (+ ymax (/ y-range 20)))))))
  plot)

(defmethod update-plot :after ((plotter gnuplot) (plot plot))
  (force-output (plot-stream-of plotter))
  plot)

(defun run-gnuplot (&key debug)
  "Starts a new Gnuplot process and returns a CLOS object of class
GNUPLOT."
  (make-instance 'gnuplot
                 :program "gnuplot" :args '("-display" ":0.0")
                 :input :stream :output :stream :search t :wait nil :debug debug))

(defun stop-gnuplot (plotter)
  "Stops the PLOTTER process."
  (let ((stream (plot-stream-of plotter)))
    (write-line "quit" stream)
    (force-output stream))
  (wait-for plotter)
  (close-process plotter)
  plotter)

(defun series-slot-range (series slot)
  "Returns a list of two integers describing the range of values in
SLOT of SERIES. The first integer is the minimum and the second the
maximum."
  (let* ((value (slot-value series slot))
         (seq (if (listp value)
                  value
                (coerce value 'list))))
    (if seq
        (list (apply #'min seq) (apply #'max seq))
      (list nil nil))))

(defun series-aggregate-range (list accessor)
  "Returns a list of describing the aggregate range of values in LIST
of series, vai ACCESSOR. The first integer is the minimum and the
second the maximum."
  (let ((ranges (mapcar accessor list)))
    (list (reduce #'min ranges :key #'first)
          (reduce #'max ranges :key #'second))))
