
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

(test find-line/line-buffer
  (with-open-file (stream "data/test3.txt"
                   :direction :input
                   :element-type '(unsigned-byte 8))
    (let ((lb (make-line-buffer stream))
          (lines (mapcar #'as-bytes '("abc"
                                      "def"
                                      "ghi"
                                      "jkl"
                                      "mno"
                                      "pqr"
                                      "stu"
                                      "vwx"
                                      "yz"))))
      ;; Success at line 1 of max 1 -> "abc"
      (multiple-value-bind (line found line-count)
          (find-line lb #'(lambda (a)
                            (= (aref a 0) (char-code #\a))) 1)
        (is (equalp (nth 0 lines) line))
        (is-true found)
        (is (= 1 line-count)))
      ;; Fail at line 1 of max 1 -> "def"
      (multiple-value-bind (line found line-count)
          (find-line lb #'(lambda (a)
                            (= (aref a 0) (char-code #\Z))) 1)
        (is (equalp (nth 1 lines) line))
        (is-false found)
        (is (= 1 line-count)))
      ;; Success at line 3 of max 4
      (multiple-value-bind (line found line-count)
          (find-line lb #'(lambda (a)
                            (= (aref a 0) (char-code #\m))) 4)
        (is (equalp (nth 4 lines) line))
        (is-true found)
        (is (= 3 line-count)))
      ;; Fall through to eof
      (multiple-value-bind (line found line-count)
          (find-line lb #'(lambda (a)
                            (declare (ignore a))
                            nil) 99)
        (is (null line))
        (is-false found)
        (is (= 5 line-count))))))