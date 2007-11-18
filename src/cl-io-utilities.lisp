
(in-package :cl-io-utilities)

(defun load-from-realign-file (filepath)
  (with-open-file (stream filepath
                          :direction :input
                          :element-type '(unsigned-byte 8))
    (let ((br (make-line-reader stream)))
      (do ((line (read-line-bytes br) (read-line-bytes br))
           (line-count 0 (1+ line-count)))
          ((null line) line-count)
       line))))

(defclass buffered-reader ()
  ((stream :initarg :stream
           :initform nil
           :reader stream-of
           :documentation "The underlying stream from which lines of
bytes are read, via the buffer.")
   (buffer :initarg :buffer
           :initform nil
           :reader buffer-of
           :documentation "The buffer from which lines are read."))
  (:documentation "A reader that allows buffered reading of lines from
a stream."))

(defmethod initialize-instance :after ((obj buffered-reader) &key)
  (fill-buffer (buffer-of obj) (stream-of obj)))

(defgeneric read-line-chars (buffered-reader)
  (:documentation "Reads up to the next newline or end of stream,
returning them as an array. The newline is not included. Returns two
values - the array and either T or NIL to indicate whether a
terminating newline was missing."))

(defgeneric read-line-bytes (buffered-reader)
  (:documentation "Reads up to the next newline or end of stream,
returning them as an array. The newline is not included. Returns two
values - the array and either T or NIL to indicate whether a
terminating newline was missing."))

(defmethod read-line-chars ((obj buffered-reader))
  (multiple-value-bind (chunks has-newline)
      (read-chunks (buffer-of obj) (stream-of obj))
    (cond ((null chunks)
           (values nil nil))
          ((zerop (length (first chunks)))
           (make-array 0 :element-type 'base-char))
          ((= 1 (length chunks))
           (values (array-copy-convert (first chunks)
                                       (make-array (length (first chunks))
                                                   :element-type 'base-char))
                   has-newline))
          (t
           (values (concatenate-chunks-convert chunks) has-newline)))))

(defmethod read-line-bytes ((obj buffered-reader))
  (multiple-value-bind (chunks has-newline)
      (read-chunks (buffer-of obj) (stream-of obj))
    (cond ((null chunks)
           (values nil nil))
          ((zerop (length (first chunks)))
           (make-array 0 :element-type 'base-char))
          ((= 1 (length chunks))
           (values (first chunks) has-newline))
          (t
           (values (concatenate-chunks chunks) has-newline)))))

(defun make-line-reader (stream &optional (nl-char #\Newline))
  (make-instance 'buffered-reader :stream stream
                 :buffer (make-byte-buffer :nl-code (char-code nl-char))))
 
(defstruct byte-buffer
  (nl-code 0 :type fixnum)
  (bytes (make-array 4096 :element-type
                     '(unsigned-byte 8) :initial-element 0)
         :type (simple-array (unsigned-byte 8) (4096)))
  (num-bytes 0 :type (integer 0 4096))
  (offset 0 :type (integer 0 4096)))

(defun is-empty-p (bb)
  "Returns TRUE if the byte-buffer BB is empty."
  (= (byte-buffer-offset bb) (byte-buffer-num-bytes bb)))

(defun fill-buffer (bb stream)
  "Fills the byte-buffer BB from the stream, returning the number of
bytes read."
  (setf (byte-buffer-offset bb) 0)
  (setf (byte-buffer-num-bytes bb)
        (read-sequence (byte-buffer-bytes bb) stream)))

(defun read-chunks (bb stream)
  "Reads chunks of bytes from STREAM viabyte-buffer BB, up to the next
newline or end of stream, returning them in a list. The newline is not
included. Returns two values - a list of chunks and either NIL or T to
indicate whether a terminating newline was missing. When the stream
underlying the buffer is exhausted the list of chunks will be empty."
  (declare (optimize (speed 3) (safety 1) (debug 0)))
  (let ((nl-position (position (byte-buffer-nl-code bb)
                               (byte-buffer-bytes bb)
                               :start (byte-buffer-offset bb)
                               :end (byte-buffer-num-bytes bb))))
    (cond ((and nl-position
                (plusp (- nl-position (byte-buffer-offset bb))))
           ;; There is a newline in the buffer, but not at the zeroth
           ;; position. Make a chunk and copy up to the newline into
           ;; it. Move the offset beyond the newline. Fill the buffer
           ;; if necessary.
           (let ((chunk (make-array (- nl-position (byte-buffer-offset bb))
                                    :element-type '(unsigned-byte 8))))
             (array-copy (byte-buffer-bytes bb) chunk
                         :source-start (byte-buffer-offset bb)
                         :source-end (1- nl-position))
             (setf (byte-buffer-offset bb) (1+ nl-position))
             (when (is-empty-p bb)
               (fill-buffer bb stream))
             (values (list chunk) nil)))
          ((and nl-position
                (zerop (- nl-position (byte-buffer-offset bb))))
           ;; There is a newline in the buffer at the zeroth
           ;; position. Make an empty chunk (for sake of
           ;; consistency). Move the offset beyond the newline. Fill
           ;; the buffer if necessary.
           (let ((chunk (make-array 0 :element-type '(unsigned-byte 8))))
             (setf (byte-buffer-offset bb) (1+ nl-position))
             (when (is-empty-p bb)
               (fill-buffer bb stream))
             (values (list chunk) nil)))
          ((zerop (byte-buffer-num-bytes bb))
           ;; The buffer is empty
           (values nil t))
          (t
           ;; There is no newline in the buffer. Make a chunk to
           ;; contain the rest of the buffered bytes and copy into
           ;; it. Fill the buffer. Recursively call read chunks to
           ;; search for the next newline.
           (let ((chunk (make-array (- (byte-buffer-num-bytes bb)
                                       (byte-buffer-offset bb))
                                    :element-type '(unsigned-byte 8)))
                 (chunks nil)
                 (missing-nl t))
             (array-copy (byte-buffer-bytes bb) chunk
                         :source-start (byte-buffer-offset bb)
                         :source-end (1- (byte-buffer-num-bytes bb)))
             (fill-buffer bb stream)
             (multiple-value-setq (chunks missing-nl)
               (read-chunks bb stream))
             (values (cons chunk chunks) missing-nl))))))

(defun concatenate-chunks (chunks)
  "Concatenates the list of byte arrays CHUNKS by copying their
contents into a new fixed length array, which is returned."
  (let ((line (make-array (reduce #'+ chunks :key #'length)
                          :element-type 'base-char)))
    (loop for chunk in chunks
          for chunk-length = (length chunk)
          with offset = 0
          do (unless (zerop chunk-length)
               (array-copy-convert chunk line :dest-start offset)
               (incf offset chunk-length)))
    line))

(defun concatenate-chunks-convert (chunks)
  "Concatenates the list of byte arrays CHUNKS by copying their
contents into a new fixed length array, which is returned."
  (let ((line (make-array (reduce #'+ chunks :key #'length)
                          :element-type '(unsigned-byte 8))))
    (loop for chunk in chunks
          for chunk-length = (length chunk)
          with offset = 0
          do (unless (zerop chunk-length)
               (array-copy chunk line :dest-start offset)
               (incf offset chunk-length)))
    line))

(defun array-copy (source dest &key (source-start 0)
                   (source-end (1- (length source))) (dest-start 0))
  "Copies bytes between SOURCE indices SOURCE-START and SOURCE-END to
DEST, inserting them into DEST at DEST-START, finally returning DEST."
  (declare (optimize (speed 3) (safety 1) (debug 0)))
  (declare (type (simple-array (unsigned-byte 8)) source dest))
  (declare (type fixnum source-start source-end dest-start))
  (when (> source-start source-end)
    (error "SOURCE-START ~a was greater than SOURCE-END ~a~%"
           source-start source-end))
  (loop for si from source-start to source-end
        for di = dest-start then (1+ di)
        do (setf (aref dest di) (aref source si)))
  dest)

(defun array-copy-convert (source dest &key (source-start 0)
                           (source-end (1- (length source))) (dest-start 0))
  "Copies elts between SOURCE indices SOURCE-START and SOURCE-END to
DEST, inserting them into DEST at DEST-START, finally returning DEST."
  (declare (optimize (speed 3) (safety 1) (debug 0)))
  (declare (type (simple-array (unsigned-byte 8)) source))
  (declare (type (simple-array base-char) dest))
  (declare (type fixnum source-start source-end dest-start))
  (when (> source-start source-end)
    (error "SOURCE-START ~a was greater than SOURCE-END ~a~%"
           source-start source-end))
  (loop for si from source-start to source-end
        for di = dest-start then (1+ di)
        do (setf (aref dest di) (code-char (aref source si))))
  dest)
