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
    :version "0.2.0"
    :licence "GPL v3"
    :depends-on (:cl-gp-utilities :split-sequence :trivial-gray-streams
                                  :cl-fad :getopt)
    :components
    ((:module :core
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
                           (:file "gnuplot"))
              :depends-on (:core))))


(in-package #:asdf)

(defmethod perform ((op test-op) (c (eql (find-system
                                          :cl-io-utilities))))
  (operate 'load-op :cl-io-utilities-test)
  (let ((*default-pathname-defaults* (component-pathname c)))
    (funcall (intern (string :run!) (string :fiveam))
             'cl-io-utilities-system:testsuite)))

(defmethod operation-done-p ((op test-op) (c (eql (find-system
                                                   :cl-io-utilities))))
  nil)

(defmethod perform ((op cldoc-op) (c (eql (find-system
                                           :cl-io-utilities))))
  (unless (find-package :cl-io-utilities)
    (operate 'load-op :cl-io-utilities))
  (let ((*default-pathname-defaults* (component-pathname c))
        (fn-sym (intern (string ":extract-documentation") (string ":cldoc")))
        (op-sym (intern (string ":html") (string ":cldoc"))))
    (funcall fn-sym op-sym "./doc/html" c)))
