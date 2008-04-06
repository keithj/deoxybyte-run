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

(in-package :cl-io-utilities)

(defconstant +byte-buffer-size+ 8192
  "Buffer size for binary-line-input-stream internal buffer.")

(deftype byte-buffer ()
  "Buffer type for binary-line-input-stream internal buffer."
  `(simple-array (unsigned-byte 8) (,+byte-buffer-size+)))

(deftype byte-buffer-index ()
  "Index type for binary-line-input-stream internal buffer."
  `(integer 0 ,+byte-buffer-size+))


(defclass line-input-stream ()
  ((line-stack :initform nil
               :accessor line-stack-of
               :documentation "A list of lines that have been pushed
back into the stream to be read again."))
  (:documentation "A line-based stream that allows lines to be pushed
back into a stack to be re-read."))

(defclass character-line-input-stream (wrapped-stream
                                       line-input-stream
                                       fundamental-character-input-stream)
  ()
  (:documentation "A line-input-stream whose lines are strings."))

(defclass binary-line-input-stream (wrapped-stream
                                    line-input-stream
                                    fundamental-binary-input-stream)
  ((buffer :initarg :buffer
           :initform nil
           :reader buffer-of
           :documentation "The buffer from which lines are read.")
   (nl-code :initarg :nl-code
            :reader nl-code-of
            :documentation "The newline character code.")
   (num-bytes :initform 0
              :accessor num-bytes-of
              :documentation "The number of bytes that were read into
the buffer from the stream.")
   (offset :initform 0
           :accessor offset-of
           :documentation "The offset in the byte buffer from which
the next byte is to be read."))
  (:documentation "A line-input-stream whose lines are arrays of
bytes. Allows buffered reading of lines of (unsigned-byte 8) from a
stream."))


;;; line-input-stream generic functions
(defgeneric more-lines-p (line-input-stream)
  (:documentation "Returns T if LINE-INPUT-STREAM contains unread
data."))

(defgeneric push-line (line-input-stream line)
  (:documentation "Pushes LINE back into LINE-INPUT-STREAM."))

(defgeneric find-line (line-input-stream test &optional max-lines)
  (:documentation "Iterates through lines read from LINE-INPUT-STREAM
until a line matching predicate TEST is found or until a number of
lines equal to MAX-LINES have been examined."))


;;; binary-line-input-stream generic functions
(defgeneric read-chunks (binary-line-input-stream)
  (:documentation "Reads chunks of bytes up to the next newline or end
of stream, returning them in a list. The newline is not
included. Returns two values - a list of chunks and either NIL or T to
indicate whether a terminating newline was missing. When the stream
underlying the buffer is exhausted the list of chunks will be empty."))

(defgeneric buffer-empty-p (binary-line-input-stream)
  (:documentation "Returns T if the internal byte buffer of
BINARY-LINE-INPUT-STREAM is empty."))

(defgeneric fill-buffer (binary-line-input-stream)
  (:documentation "Fills the byte buffer of BINARY-LINE-INPUT-STREAM
from its stream, setting the num-bytes slot to the number of bytes
actually read."))


;;; line-input-stream constructor
(defun make-line-input-stream (stream)
  "Returns a new CHARACTER-LINE-INPUT-STREAM or
BINARY-LINE-INPUT-STREAM wrapping STREAM. The element type of STREAM
must be either CHARACTER or (UNSIGNED-BYTE 8)."
  (unless (and (streamp stream)
               (input-stream-p stream)
               (open-stream-p stream))
    (error 'invalid-argument-error
           :params 'stream :args stream
           :text "expected an open input-stream"))
  (let ((elt-type (stream-element-type stream)))
    (cond ((subtypep elt-type 'character)
           (make-instance 'character-line-input-stream
                          :stream stream))
          ((equal '(unsigned-byte 8) elt-type)
           (make-instance 'binary-line-input-stream
                          :stream stream
                          :nl-code (char-code #\Newline)
                          :buffer (make-array +byte-buffer-size+
                                              :element-type '(unsigned-byte 8)
                                              :initial-element 0)))
          (t
           (error "Invalid element type ~a from stream ~a."
                  elt-type stream)))))


;;; wrapped-stream methods
(defmethod stream-element-type ((stream wrapped-stream))
  (stream-element-type (stream-of stream)))

(defmethod close ((stream wrapped-stream) &key abort)
  (close (stream-of stream) :abort abort))

(defmethod open-stream-p ((stream wrapped-stream))
  (open-stream-p (stream-of stream)))

(defmethod stream-file-position ((stream wrapped-stream))
  (file-position (stream-of stream)))


;;; line-input-stream methods
(defmethod find-line ((stream line-input-stream) test
                      &optional max-lines)
  (do* ((line (stream-read-line stream) (stream-read-line stream))
        (matching-line-p (and (vectorp line) (funcall test line))
                         (and (vectorp line) (funcall test line)))
        (line-count 1 (1+ line-count)))
       ((or (eql :eof line)
            matching-line-p
            (and (not (null max-lines))
                 (= line-count max-lines)))
        (values line matching-line-p line-count))))


;;; character-line-input-stream methods
(defmethod stream-clear-input ((stream character-line-input-stream))
  (setf (line-stack-of stream) nil))

(defmethod stream-read-char ((stream character-line-input-stream))
  (stream-clear-input stream)
  (read-char (stream-of stream)))

(defmethod stream-unread-char ((stream character-line-input-stream)
                               (char character))
  (stream-clear-input stream)
  (unread-char char (stream-of stream)))

(defmethod stream-read-line ((stream character-line-input-stream))
  (if (null (line-stack-of stream))
      (multiple-value-bind (line missing-newline-p)
          (read-line (stream-of stream) nil nil)
        (values line missing-newline-p))
    (pop (line-stack-of stream))))

(defmethod more-lines-p ((stream character-line-input-stream))
  (or (line-stack-of stream)
      (peek-char nil (stream-of stream) :eof nil)))

(defmethod push-line ((stream character-line-input-stream) (line string))
  (push line (line-stack-of stream)))


;;; binary-line-input-stream methods
(defmethod stream-clear-input ((stream binary-line-input-stream))
  (setf (offset-of stream) 0
        (num-bytes-of stream) 0
        (line-stack-of stream) nil))

(defmethod stream-read-byte ((stream binary-line-input-stream))
  (stream-clear-input stream)
  (read-byte (stream-of stream)))

(defmethod stream-read-line ((stream binary-line-input-stream))
  (unless (open-stream-p (stream-of stream))
    (error "Stream is closed."))
  (if (null (line-stack-of stream))
      (multiple-value-bind (chunks has-newline-p)
          (read-chunks stream)
        (cond ((null chunks)
               (values :eof t))
              ((zerop (length (first chunks)))
               (first chunks))
              ((= 1 (length chunks))
               (values (first chunks) has-newline-p))
              (t
               (values (concatenate-chunks chunks) has-newline-p))))
    (pop (line-stack-of stream))))

(defmethod stream-file-position ((stream binary-line-input-stream))
  (- (file-position (stream-of stream))
     (- (num-bytes-of stream) (offset-of stream))))

(defmethod more-lines-p :before ((stream binary-line-input-stream))
  (when (zerop (num-bytes-of stream))
    (read-chunks stream)))

(defmethod more-lines-p ((stream binary-line-input-stream))
  (or (line-stack-of stream)
      (not (zerop (num-bytes-of stream)))))

(defmethod push-line ((stream binary-line-input-stream) (line vector))
  (push line (line-stack-of stream)))

(defmethod buffer-empty-p ((stream binary-line-input-stream))
  (= (offset-of stream) (num-bytes-of stream)))

(defmethod fill-buffer ((stream binary-line-input-stream))
  (setf (offset-of stream) 0
        (num-bytes-of stream) (read-sequence (buffer-of stream)
                                             (stream-of stream))))

(defmethod read-chunks :before ((stream binary-line-input-stream))
  (when (buffer-empty-p stream)
    (fill-buffer stream)))

(defmethod read-chunks ((stream binary-line-input-stream))
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (let ((offset (offset-of stream))
        (num-bytes (num-bytes-of stream))
        (buffer (buffer-of stream)))
    (declare (type byte-buffer buffer)
             (type byte-buffer-index offset num-bytes))
    (let ((nl-position (position (nl-code-of stream) buffer
                                 :start offset :end num-bytes)))
      (cond ((and nl-position
                  (plusp (- nl-position offset)))
           ;; There is a newline in the buffer, but not at the zeroth
           ;; position. Make a chunk and copy up to the newline into
           ;; it. Move the offset beyond the newline. Fill the buffer
           ;; if necessary.
             (let ((chunk (make-array (- nl-position offset)
                                      :element-type '(unsigned-byte 8))))
               (gpu:copy-array buffer offset (1- nl-position)
                               chunk 0)
               (setf (offset-of stream) (1+ nl-position))
               (values (list chunk) nil)))
            ((and nl-position
                  (zerop (- nl-position offset)))
             ;; There is a newline in the buffer at the zeroth
             ;; position. Make an empty chunk (for sake of
             ;; consistency). Move the offset beyond the newline. Fill
             ;; the buffer if necessary.
             (let ((chunk (make-array 0 :element-type '(unsigned-byte 8))))
               (setf (offset-of stream) (1+ nl-position))
               (values (list chunk) nil)))
            ((zerop num-bytes)
             ;; The buffer is empty
             (values nil t))
            (t
             ;; There is no newline in the buffer. Make a chunk to
             ;; contain the rest of the buffered bytes and copy into
             ;; it. Fill the buffer. Recursively call read chunks to
             ;; search for the next newline.
             (let ((chunk (make-array (- num-bytes offset)
                                      :element-type '(unsigned-byte 8)))
                   (chunks nil)
                   (missing-nl-p t))
               (gpu:copy-array buffer offset (1- num-bytes)
                               chunk 0)
               (fill-buffer stream)
               (multiple-value-setq (chunks missing-nl-p)
                 (read-chunks stream))
               (values (cons chunk chunks) missing-nl-p)))))))

(defun concatenate-chunks (chunks)
  "Concatenates the list of byte arrays CHUNKS by copying their
contents into a new fixed length array, which is returned."
  (let ((line (make-array (reduce #'+ chunks :key #'length)
                          :element-type '(unsigned-byte 8))))
    (loop
       for chunk of-type (simple-array (unsigned-byte 8)) in chunks
       for chunk-length = (length chunk)
       with offset = 0
       do (unless (zerop chunk-length)
            (gpu:copy-array chunk 0 (1- chunk-length)
                            line offset)
            (incf offset chunk-length)))
    line))
