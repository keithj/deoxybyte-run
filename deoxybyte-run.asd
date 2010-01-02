;;;
;;; Copyright (C) 2007-2010 Keith James. All rights reserved.
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

(in-package :cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (asdf:find-system :deoxybyte-systems nil)
    (asdf:operate 'asdf:load-op :deoxybyte-systems)))

(defpackage :uk.co.deoxybyte-run-system
  (:use :common-lisp :asdf)
  (:import-from :deoxybyte-systems :lift-test-config :cldoc-config))

(in-package :uk.co.deoxybyte-run-system)

(defsystem deoxybyte-run
    :name "deoxybyte-run"
    :version "0.4.2"
    :author "Keith James"
    :licence "GPL v3"
    :in-order-to ((test-op (load-op :deoxybyte-run :deoxybyte-run-test)))
    :depends-on ((:version :deoxybyte-io "0.5.3")
                 (:version :deoxybyte-utilities "0.5.6"))
    :components
    ((:module :core
              :serial t
              :pathname "src/"
              :components ((:file "package")
                           (:file "generics")
                           (:file "conditions")
                           (:file "deoxybyte-run")
                           (:file "gnuplot")
                           (:file "rsh")))
     (:lift-test-config :deoxybyte-run-test
                        :target-system :deoxybyte-run)
     (:cldoc-config :deoxybyte-run-doc
                    :target-system :deoxybyte-run
                    :pathname "doc/html/")))
