;;;
;;; Copyright (C) 2007-2008, Keith James. All rights reserved.
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

(defpackage #:cl-io-utilities-system
  (:use :common-lisp :asdf)
  (:export #:testsuite))


(in-package #:cl-io-utilities-system)

(defsystem cl-io-utilities
    :name "cl-io-utilities"
    :author "Keith James"
    :version "0.1.0"
    :licence "GPL v3"
    :depends-on (:cl-gp-utilities :trivial-gray-streams)
    :components
    ((:module :cl-io-utilities
              :serial t
              :pathname "src/"
              :components ((:file "package")
                           (:file "conditions")
                           (:file "parse-float")
                           (:file "line-input-stream")
                           (:file "simple-table-parser")))))


(in-package #:asdf)

(defmethod perform ((op test-op) (c (eql (find-system
                                          'cl-io-utilities))))
  (operate 'load-op :cl-io-utilities-test)
  (let ((*default-pathname-defaults* (component-pathname c)))
    (funcall (intern (string :run!) (string :fiveam))
             'cl-io-utilities-system:testsuite)))

(defmethod operation-done-p ((op test-op) (c (eql (find-system
                                                   'cl-io-utilities))))
  nil)
