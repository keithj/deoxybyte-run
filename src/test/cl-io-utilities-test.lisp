
(in-package :cl-io-utilities-test)

(defun as-bytes (str)
  (make-array (length str) :element-type '(unsigned-byte 8)
              :initial-contents (loop for c across str
                                     collect (char-code c))))

(defun as-chars (bytes)
  (make-array (length bytes) :element-type 'base-char
              :initial-contents (loop for b across bytes
                                     collect (code-char b))))

(define-test pull-line/byte-line-buffer
    (with-open-file (stream "data/test1.txt"
                     :direction :input
                     :element-type '(unsigned-byte 8))
      (let ((lb (make-line-buffer stream :element-type '(unsigned-byte 8)))
            (line "abcdefghijklmnopqrstuvwxyz"))
        (multiple-value-bind (b missing-newline-p)
            (pull-line lb)
          (assert-equal line (as-chars b))
          (assert-false missing-newline-p)))))

(define-test pull-line/char-line-buffer
    (with-open-file (stream "data/test1.txt"
                     :direction :input
                     :element-type '(unsigned-byte 8))
      (let ((lb (make-line-buffer stream :element-type 'base-char))
            (line "abcdefghijklmnopqrstuvwxyz"))
        (multiple-value-bind (c missing-newline-p)
            (pull-line lb)
          (assert-equal line c)
          (assert-false missing-newline-p)))))

(define-test push-line/byte-line-buffer
    (with-open-file (stream "data/test1.txt"
                     :direction :input
                     :element-type '(unsigned-byte 8))
      (let ((lb (make-line-buffer stream :element-type 'base-char))
            (line "abcdefghijklmnopqrstuvwxyz"))
        (multiple-value-bind (c missing-newline-p)
            (pull-line lb)
          (assert-equal line c)
          (assert-false missing-newline-p)
          (push-line lb c)
          (assert-equal line (pull-line lb))))))
