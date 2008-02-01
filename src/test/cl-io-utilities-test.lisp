
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

;;; Gray-streams methods to be tested

;; input streams
;; sb-gray:stream-clear-input stream
;; sb-gray:stream-read-sequence stream seq &optional start end

;; binary streams
;; sb-gray:stream-read-byte stream
;; sb-gray:stream-write-byte stream integer

;; character input streams
;; sb-gray:stream-peek-char stream
;; sb-gray:stream-read-char-no-hang stream
;; sb-gray:stream-read-char stream
;; sb-gray:stream-read-line stream
;; sb-gray:stream-listen stream
;; sb-gray:stream-unread-char stream character

(test gray-common-methods/character-line-input-stream
  "Test universal gray-streams methods."
  (with-open-file (stream (merge-pathnames "data/test1.txt")
                   :direction :input
                   :element-type 'base-char
                   :external-format :ascii)
    (let ((s (make-line-input-stream stream)))
      (is-true (subtypep (stream-element-type s) 'character))
      (is (eql nil (stream-file-position s)))
      (is-true (open-stream-p s))
      (is-true (close s))
      (is-false (open-stream-p s))
      (signals error
        (stream-read-line s)))))

(test gray-common-methods/binary-line-input-stream
  "Test universal gray-streams methods."
  (with-open-file (stream (merge-pathnames "data/test1.txt")
                   :direction :input
                   :element-type '(unsigned-byte 8))
    (let ((s (make-line-input-stream stream)))
      (is (equal (stream-element-type s) '(unsigned-byte 8)))
      (is (eql nil (stream-file-position s)))
      (is-true (open-stream-p s))
      (is-true (close s))
      (is-false (open-stream-p s))
      (signals error
        (stream-read-line s)))))

(test gray-input-methods/character-line-input-stream
  "Test input-stream gray-streams methods."
    (with-open-file (stream (merge-pathnames "data/test1.txt")
                   :direction :input
                   :element-type 'base-char
                   :external-format :ascii)
    (let ((s (make-line-input-stream stream))
          (b (make-array 10 :element-type 'character)))
      ;; stream-clear-input should empty the line buffer
      (push-line s "aaaa")
      (is-true (iou::line-stack-of s))
      (is (eql nil (stream-clear-input s)))
      (is-false (iou::line-stack-of s))
      (is (= 10 (stream-read-sequence s b)))
      (is (string= "abcdefghij" b)))))

(test gray-input-methods/binary-line-input-stream
  "Test input-stream gray-streams methods."
    (with-open-file (stream (merge-pathnames "data/test1.txt")
                   :direction :input
                   :element-type '(unsigned-byte 8))
    (let ((s (make-line-input-stream stream))
          (b (make-array 10 :element-type '(unsigned-byte 8))))
      (is (eql nil (stream-clear-input s)))
      (is (= 10 (stream-read-sequence s b)))
      (is (equalp (as-bytes "abcdefghij") b)))))

(test gray-binary-methods/binary-line-input-stream
  "Test binary-stream gray-streams methods."
  (with-open-file (stream (merge-pathnames "data/test1.txt")
                   :direction :input
                   :element-type '(unsigned-byte 8))
    (let ((s (make-line-input-stream stream))
          (line (as-bytes "abcdefghijklmnopqrstuvwxyz")))
      (loop for byte across line
           do (is (= byte (stream-read-byte s)))))))

(test gray-character-methods/character-line-input-stream
  "Test character-stream gray-streams methods."
  (with-open-file (stream (merge-pathnames "data/test1.txt")
                   :direction :input
                   :element-type 'base-char
                   :external-format :ascii)
    (let ((s (make-line-input-stream stream))
          (line "abcdefghijklmnopqrstuvwxyz"))
      (is (char= #\a (stream-read-char s)))
      (is (eql nil (stream-unread-char s #\a)))
      (loop for char across line
         do (is (char= char (stream-read-char s)))))))

(test stream-read-line/character-line-input-stream
  "Test stream-read-line."
  (with-open-file (stream (merge-pathnames "data/test1.txt")
                   :direction :input
                   :element-type 'base-char
                   :external-format :ascii)
    (let ((s (make-line-input-stream stream))
          (line "abcdefghijklmnopqrstuvwxyz"))
      (multiple-value-bind (line2 missing-newline-p)
          (stream-read-line s)
        (is (equalp line2 line))
        (is-false missing-newline-p))
      (multiple-value-bind (line2 missing-newline-p)
          (stream-read-line s)
        (is-false line2)
        (is-true missing-newline-p)))))

(test stream-read-line/binary-line-input-stream
  "Test stream-read-line."
  (with-open-file (stream (merge-pathnames "data/test1.txt")
                   :direction :input
                   :element-type '(unsigned-byte 8))
    (let ((s (make-line-input-stream stream))
          (line (as-bytes "abcdefghijklmnopqrstuvwxyz")))
      (multiple-value-bind (bytes missing-newline-p)
          (stream-read-line s)
        (is (equalp line bytes))
        (is-false missing-newline-p))
      (multiple-value-bind (bytes missing-newline-p)
          (stream-read-line s)
        (is-false bytes)
        (is-true missing-newline-p)))))

(test push-line/character-line-input-stream
  (with-open-file (stream (merge-pathnames "data/test1.txt")
                   :direction :input
                   :element-type 'base-char
                   :external-format :ascii)
    (let ((s (make-line-input-stream stream))
          (line "abcdefghijklmnopqrstuvwxyz"))
      (multiple-value-bind (line2 missing-newline-p)
          (stream-read-line s)
        (is (equalp line line2))
        (is-false missing-newline-p)
        (push-line s line2)
        (is (equalp line (stream-read-line s)))))))

(test push-line/binary-line-input-stream
  (with-open-file (stream (merge-pathnames "data/test1.txt")
                   :direction :input
                   :element-type '(unsigned-byte 8))
    (let ((s (make-line-input-stream stream))
          (line (as-bytes "abcdefghijklmnopqrstuvwxyz")))
      (multiple-value-bind (bytes missing-newline-p)
          (stream-read-line s)
        (is (equalp line bytes))
        (is-false missing-newline-p)
        (push-line s bytes)
        (is (equalp line (stream-read-line s)))))))

;; ;; This test fails on SBCL due to a bug in read-line (missing-newline-p)
;; (test missing-newline-p/line-buffer
;;   (with-open-file (stream (method-name "data/test2.txt")
;;                    :direction :input
;;                    :element-type 'base-char
;;                    :external-format :ascii)
;;     (let ((lb (make-line-buffer stream))
;;           (lines '("1234567890"
;;                    "0987654321"
;;                    "abcdefghij"
;;                    "klmnopqrst")))
;;       (dolist (line (butlast lines))
;;         (multiple-value-bind (line2 missing-newline-p)
;;             (pull-line lb)
;;           (is (equalp line line2))
;;           (is-false missing-newline-p)))
;;       (multiple-value-bind (line2 missing-newline-p)
;;           (pull-line lb)
;;         (is (equalp (car (last lines)) line2))
;;         (is-true missing-newline-p)))))

(test missing-newline-p/binary-line-input-stream
  (with-open-file (stream (merge-pathnames "data/test2.txt")
                   :direction :input
                   :element-type '(unsigned-byte 8))
    (let ((s (make-line-input-stream stream))
          (lines (mapcar #'as-bytes '("1234567890"
                                      "0987654321"
                                      "abcdefghij"
                                      "klmnopqrst"))))
      (dolist (line (butlast lines))
        (multiple-value-bind (bytes missing-newline-p)
            (stream-read-line s)
          (is (equalp line bytes))
          (is-false missing-newline-p)))
      (multiple-value-bind (bytes missing-newline-p)
          (stream-read-line s)
        (is (equalp (car (last lines)) bytes))
        (is-true missing-newline-p)))))

(test find-line/binary-line-input-stream
  (with-open-file (stream (merge-pathnames "data/test3.txt")
                   :direction :input
                   :element-type '(unsigned-byte 8))
    (let ((s (make-line-input-stream stream))
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
          (find-line s #'(lambda (a)
                            (= (aref a 0) (char-code #\a))) 1)
        (is (equalp (nth 0 lines) line))
        (is-true found)
        (is (= 1 line-count)))
      ;; Fail at line 1 of max 1 -> "def"
      (multiple-value-bind (line found line-count)
          (find-line s #'(lambda (a)
                            (= (aref a 0) (char-code #\Z))) 1)
        (is (equalp (nth 1 lines) line))
        (is-false found)
        (is (= 1 line-count)))
      ;; Success at line 3 of max 4
      (multiple-value-bind (line found line-count)
          (find-line s #'(lambda (a)
                            (= (aref a 0) (char-code #\m))) 4)
        (is (equalp (nth 4 lines) line))
        (is-true found)
        (is (= 3 line-count)))
      ;; Fall through to eof
      (multiple-value-bind (line found line-count)
          (find-line s #'(lambda (a)
                            (declare (ignore a))
                            nil))
        (is (null line))
        (is-false found)
        (is (= 5 line-count))))))
