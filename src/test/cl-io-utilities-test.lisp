
(in-package :cl-io-utilities-system)

(fiveam:def-suite testsuite
    :description "The test suite.")


(in-package :cl-io-utilities-test)

(defun as-bytes (str)
  (make-array (length str) :element-type '(unsigned-byte 8)
              :initial-contents (loop for c across str
                                     collect (char-code c))))

(defun as-chars (bytes)
  (make-array (length bytes) :element-type 'base-char
              :initial-contents (loop for b across bytes
                                     collect (code-char b))))

(in-suite cl-io-utilities-system:testsuite)

(test pull-line/line-buffer
  (with-open-file (stream "data/test1.txt"
                   :direction :input
                   :element-type '(unsigned-byte 8))
    (let ((lb (make-line-buffer stream))
          (line (as-bytes "abcdefghijklmnopqrstuvwxyz")))
      (multiple-value-bind (bytes missing-newline-p)
          (pull-line lb)
        (is (equalp line bytes))
        (is-false missing-newline-p)
        (multiple-value-bind (bytes missing-newline-p)
            (pull-line lb)
          (is-false bytes)
          (is-false missing-newline-p))))))

(test push-line/line-buffer
  (with-open-file (stream "data/test1.txt"
                   :direction :input
                   :element-type '(unsigned-byte 8))
    (let ((lb (make-line-buffer stream))
          (line (as-bytes "abcdefghijklmnopqrstuvwxyz")))
      (multiple-value-bind (bytes missing-newline-p)
          (pull-line lb)
        (is (equalp line bytes))
        (is-false missing-newline-p)
        (push-line lb bytes)
        (is (equalp line (pull-line lb)))))))

(test missing-newline-p
  (with-open-file (stream "data/test2.txt"
                   :direction :input
                   :element-type '(unsigned-byte 8))
    (let ((lb (make-line-buffer stream))
          (lines (mapcar #'as-bytes '("1234567890"
                                      "0987654321"
                                      "abcdefghij"
                                      "klmnopqrst"))))
      (dolist (line (butlast lines))
        (multiple-value-bind (bytes missing-newline-p)
            (pull-line lb)
          (is (equalp line bytes))
          (is-false missing-newline-p)))
      (multiple-value-bind (bytes missing-newline-p)
          (pull-line lb)
        (is (equalp (car (last lines)) bytes))
        (is-true missing-newline-p)))))
