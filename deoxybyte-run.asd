;;;
;;; Copyright (c) 2007-2011 Keith James. All rights reserved.
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

(asdf:load-system :deoxybyte-systems)

(in-package :uk.co.deoxybyte-systems)

(defsystem deoxybyte-run
    :name "deoxybyte-run"
    :version "0.5.0"
    :author "Keith James"
    :licence "GPL v3"
    :in-order-to ((test-op (load-op :deoxybyte-run :deoxybyte-run-test)))
    :depends-on ((:version :deoxybyte-io "0.13.0")
                 (:version :deoxybyte-utilities "0.10.0"))
    :components
    ((:module :core
              :serial t
              :pathname "src/"
              :components ((:file "package")
                           (:file "conditions")
                           (:file "deoxybyte-run")
                           #+:sbcl (:file "sbcl")
                           #+:ccl (:file "ccl")
                           #- (or :sbcl :ccl) (:file "default")
                           (:file "program-instances")
                           (:file "gnuplot")))
     (:lift-test-config :deoxybyte-run-test
                        :target-system :deoxybyte-run)
     (:cldoc-config :deoxybyte-run-doc
                    :target-system :deoxybyte-run
                    :pathname "doc/html/")))
