
(in-package :cl-user)

(defpackage #:cl-io-utilities-system
  (:use :common-lisp :asdf)
  (:export #:testsuite))


(in-package #:cl-io-utilities-system)

(defsystem cl-io-utilities
    :name "cl-io-utilities"
    :author "Keith James"
    :version "0.1.0"
    :licence "GPL"
    :depends-on (:cl-gp-utilities)
    :components
    ((:module :cl-io-utilities
              :serial t
              :pathname "src/"
              :components ((:file "package")
                           (:file "cl-io-utilities")))))


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