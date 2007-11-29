
(defsystem cl-io-utilities-test
    :depends-on (:cl-io-utilities :lisp-unit)
    :components ((:module :cl-io-utilities-test
                          :pathname "src/test/"
                          :components ((:file "package")
                                       (:file "cl-io-utilities-test"
                                              :depends-on ("package")))))
    :in-order-to ((test-op (load-op cl-io-utilities-test))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package 'lisp-unit)
    (asdf:oos 'asdf:load-op 'lisp-unit))
  (unless (find-package 'cl-io-utilities)
    (asdf:oos 'asdf:load-op 'cl-io-utilities)))

(defmethod perform ((operation test-op)
                    (component (eql (find-system 'cl-io-utilities-test))))
  (let ((*default-pathname-defaults* (component-pathname component)))
    (lisp-unit:run-all-tests :cl-io-utilities-test)))

(defmethod operation-done-p ((operation test-op)
                             (component (eql (find-system
                                              'cl-io-utilities-test))))
  (values nil))