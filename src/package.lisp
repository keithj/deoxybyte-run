;;;
;;; Copyright (c) 2008-2011 Keith James. All rights reserved.
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

(defpackage :uk.co.deoxybyte-run
  (:use #:common-lisp #:deoxybyte-utilities #:deoxybyte-io)
  (:nicknames
   #:deoxybyte-run
   #:dxr)
  (:export
   ;; External programs
   #:external-program-error
   #:non-zero-exit-error
   #:external-program
   #:program-of
   #:args-of
   #:process-of
   #:input-of
   #:output-of
   #:error-of
   #:wait-for
   #:status-of
   #:exit-code-of
   #:close-process
   #:kill-process
   #:run
   #:runningp
   #:programs-running

   ;; rsh
   #:*default-remote-host*
   #:*remote-pathname-defaults*
   #:rsh
   #:rsh-exec
   #:host-of
   #:merge-remote-pathnames
   #:rsh-list-directory
   #:rsh-file-exists-p
   #:rsh-directory-exists-p
   #:rsh-files-exist-p
   #:rsh-directories-exist-p
   #:rsh-make-directory
   #:rsh-ensure-directories-exist

   ;; Gnuplot
   #:gnuplot
   #:plot
   #:2d-plot
   #:histogram
   #:x-axis
   #:y-axis
   #:axis
   #:series
   #:xy-series
   #:category-series
   #:title-of
   #:series-of
   #:nth-series-of
   #:legend-of
   #:x-axis-of
   #:y-axis-of
   #:position-of
   #:range-of
   #:x-range-of
   #:y-range-of
   #:label-of
   #:tics-of
   #:minor-tics-of
   #:style-of
   #:x-values-of
   #:y-values-of
   #:length-of
   #:draw-plot
   #:update-plot
   #:append-data
   #:run-gnuplot
   #:stop-gnuplot)
  (:documentation "The deoxybyte-run system is a compatability and
convenience layer for running external programs. In addition, wrappers
for some software are included:

- Gnuplot
- rsh"))
