
(in-package #:cl-io-utilities-system)

(defsystem cl-io-utilities-test
  :depends-on (:cl-io-utilities :fiveam)
  :components ((:module :cl-io-utilities-test
                        :serial t
                        :pathname "src/test/"
                        :components ((:file "package")
                                     (:file "cl-io-utilities-test")))))
