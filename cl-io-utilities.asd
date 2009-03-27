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

(in-package :cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (asdf:find-system :cl-system-utilities nil)
    (asdf:operate 'asdf:load-op :cl-system-utilities)))

(defpackage #:cl-io-utilities-system
  (:use :common-lisp :asdf :cl-system-utilities))

(in-package #:cl-io-utilities-system)

(defsystem cl-io-utilities
    :name "cl-io-utilities"
    :author "Keith James"
    :version "0.2.0"
    :licence "GPL v3"
    :in-order-to ((test-op (load-op :cl-io-utilities :cl-io-utilities-test)))
    :depends-on (:cl-gp-utilities :split-sequence :cl-fad :getopt
                 :trivial-gray-streams)
    :components
    ((:module :core
              :serial t
              :pathname "src/"
              :components ((:file "package")
                           (:file "conditions")
                           (:file "parse-float")
                           (:file "streams")
                           (:file "line-input-stream")
                           (:file "command-line-interface")
                           (:file "files-and-directories")
                           (:file "simple-table-parser")
                           (:file "external-merge-sort")))
     (:module :external-programs
              :serial t
              :pathname "src/external-program/"
              :components ((:file "package")
                           (:file "external-program")
                           (:file "conditions")
                           (:file "gnuplot")
                           (:file "rsh"))
              :depends-on (:core))
     (:lift-test-config :cl-io-utilities-test
                        :target-system :cl-io-utilities)
     (:cldoc-config :cl-io-utilities-doc
                    :target-system :cl-io-utilities
                    :pathname "doc/html")))
