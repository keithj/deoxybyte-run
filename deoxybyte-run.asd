;;;
;;; Copyright (C) 2007, 2008, 2009, 2010, 2011, 2012, 2013 Keith
;;; James. All rights reserved.
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
    :version "0.7.0"
    :author "Keith James"
    :licence "GPL v3"
    :in-order-to ((test-op (load-op :deoxybyte-run :deoxybyte-run-test))
                  (doc-op (load-op :deoxybyte-run :cldoc)))
    :depends-on ((:version :deoxybyte-systems "1.0.0")
                 (:version :deoxybyte-io "0.15.0")
                 (:version :deoxybyte-utilities "0.11.0"))
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
                           (:file "gnuplot"))))
    :perform (test-op :after (op c)
                      (maybe-run-lift-tests :deoxybyte-run
                                            "deoxybyte-run-test.config"))
    :perform (doc-op :after (op c)
                     (maybe-build-cldoc-docs :deoxybyte-run "doc/html")))
