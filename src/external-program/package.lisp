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

(defpackage #:uk.co.deoxybyte-gnuplot
  (:use #:common-lisp :uk.co.deoxybyte-utilities :uk.co.deoxybyte-io)
  (:nicknames
   #:deoxybyte-gnuplot
   #:dxg
   #:gpt)
  (:export
   ;; Classes
   #:gnuplot
   #:plot
   #:2d-plot
   #:axis
   #:series
   #:xy-series

   ;; Generics
   #:title-of
   #:series-of
   #:legend-of
   #:x-axis-of
   #:y-axis-of
   #:position-of
   #:range-of
   #:label-of
   #:tics-of
   #:minor-tics-of
   #:style-of
   #:x-values-of
   #:y-values-of
   #:length-of
   #:draw-plot

   ;; Functions
   #:run-gnuplot
   #:stop-gnuplot)
  (:documentation "This package provides a simple CLOS interface for
starting the Gnuplot program in a separate process and then using it
to plot graphs in batch mode.

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
to the screen.

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
;;;                                     '(-1 0 3 4 5 8)
;;;                                     :y-values
;;;                                     '(-10 2 4 5 17 10)
;;;                                     :style '(:linespoints
;;;                                              :smooth
;;;                                              :csplines)))))
;;;   (draw-plot plotter plot :terminal :png :output "foo.png")
;;;   (draw-plot plotter plot :terminal :x11)
;;;   (sleep 5)
;;;   (stop-gnuplot plotter))"))
