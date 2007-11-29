
(in-package :cl-io-utilities)

(defclass line-buffer ()
  ((stream :initarg :stream
           :initform nil
           :reader stream-of
           :documentation "The underlying stream from which lines of
bytes are read, via the buffer.")
   (buffer :initarg :buffer
           :initform nil
           :reader buffer-of
           :documentation "The buffer from which lines are read.")
   (pushback :initform nil
             :accessor pushback-of
             :documentation "A list of lines that have been pushed
back into the reader to be read again."))
  (:documentation "Allows buffered reading of lines of bytes from a
stream."))

(defmethod initialize-instance :after ((obj line-buffer) &key)
  (fill-buffer (buffer-of obj) (stream-of obj)))

(defgeneric pull-line (line-buffer)
  (:documentation "Reads up to the next newline from LINE-BUFFER, or
end of stream, returning an array. The newline is not
included. Returns two values - the array and either T or NIL to
indicate whether a terminating newline was missing."))

(defgeneric push-line (line-buffer line)
  (:documentation "Pushes LINE back into LINE-BUFFER."))

(defmethod pull-line ((obj line-buffer))
  (if (null (pushback-of obj))
      (multiple-value-bind (chunks has-newline)
          (read-chunks (buffer-of obj) (stream-of obj))
        (cond ((null chunks)
               (values nil nil))
              ((zerop (length (first chunks)))
               (first chunks))
              ((= 1 (length chunks))
               (values (first chunks) has-newline))
              (t
               (values (concatenate-chunks chunks) has-newline))))
    (pop (pushback-of obj))))

(defmethod push-line ((obj line-buffer) (line vector))
  (push line (pushback-of obj)))

(defun make-line-buffer (stream &optional (nl-char #\Newline))
  (make-instance 'line-buffer :stream stream
                 :buffer (make-byte-buffer :nl-code (char-code nl-char))))

(defstruct byte-buffer
  (nl-code 0 :type fixnum)
  (bytes (make-array 4096 :element-type '(unsigned-byte 8)
                     :initial-element 0)
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
  "Reads chunks of bytes from STREAM via byte-buffer BB, up to the
next newline or end of stream, returning them in a list. The newline
is not included. Returns two values - a list of chunks and either NIL
or T to indicate whether a terminating newline was missing. When the
stream underlying the buffer is exhausted the list of chunks will be
empty."
  (declare (optimize (speed 3) (safety 0) (debug 0)))
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
             (copy-array (byte-buffer-bytes bb)
                         (byte-buffer-offset bb) (1- nl-position)
                         chunk 0)
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
             (copy-array (byte-buffer-bytes bb)
                         (byte-buffer-offset bb) (1- (byte-buffer-num-bytes bb))
                         chunk 0)
             (fill-buffer bb stream)
             (multiple-value-setq (chunks missing-nl)
               (read-chunks bb stream))
             (values (cons chunk chunks) missing-nl))))))

(defun concatenate-chunks (chunks)
  "Concatenates the list of byte arrays CHUNKS by copying their
contents into a new fixed length array, which is returned."
  (let ((line (make-array (reduce #'+ chunks :key #'length)
                          :element-type '(unsigned-byte 8))))
    (loop for chunk of-type (simple-array (unsigned-byte 8)) in chunks
          for chunk-length = (length chunk)
          with offset = 0
          do (unless (zerop chunk-length)
               (copy-array chunk 0 (1- chunk-length)
                           line offset)
               (incf offset chunk-length)))
    line))
