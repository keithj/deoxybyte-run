
(in-package :cl-io-utilities-test)

(defun as-bytes (str)
  (make-array (length str) :element-type '(unsigned-byte 8)
              :initial-contents (loop for c across str
                                     collect (char-code c))))

(defun as-chars (bytes)
  (make-array (length bytes) :element-type 'base-char
              :initial-contents (loop for b across bytes
                                     collect (code-char b))))

(define-test read-line-bytes
    (with-open-file (stream "data/test1.txt"
                     :direction :input
                     :element-type '(unsigned-byte 8))
      (let ((br (make-line-reader stream))
            (line "abcdefghijklmnopqrstuvwxyz"))
        (multiple-value-bind (b missing-newline-p)
            (read-line-bytes br)
          (assert-equal line (as-chars b))
          (assert-false missing-newline-p)))))

(define-test read-line-chars
    (with-open-file (stream "data/test1.txt"
                     :direction :input
                     :element-type '(unsigned-byte 8))
      (let ((br (make-line-reader stream))
            (line "abcdefghijklmnopqrstuvwxyz"))
        (multiple-value-bind (c missing-newline-p)
            (read-line-chars br)
          (assert-equal line c)
          (assert-false missing-newline-p)))))