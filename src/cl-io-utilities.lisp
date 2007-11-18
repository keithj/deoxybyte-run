
(in-package :cl-io-utilities)

(defun load-from-realign-file (filepath)
  (with-open-file (stream filepath
                          :direction :input
                          :element-type '(unsigned-byte 8))
    (let ((bb (buffer-byte-stream stream)))
      (do ((line (read-byte-line bb) (read-byte-line bb))
           (line-count 0 (1+ line-count)))
          ((null line) line-count)
        (convert-to-char line)))))

(defstruct byte-buffer
  (stream nil)
  (nl-code (char-code #\Newline) :type fixnum)
  (bytes (make-array 4096 :element-type '(unsigned-byte 8) :initial-element 0)
         :type (simple-array (unsigned-byte 8) (4096)))
  (num-bytes 0 :type (integer 0 4096))
  (offset 0 :type (integer 0 4096)))

(defun buffer-byte-stream (stream &optional (nl-char #\Newline))
  (let ((bb (make-byte-buffer :nl-code (char-code nl-char) :stream stream)))
    (fill-buffer bb)
    bb))

(defun convert-to-char (byte-line)
  (declare (optimize (speed 3) (safety 1) (debug 0)))
  (declare (type (simple-array (unsigned-byte 8)) byte-line))
  (let ((char-line (make-array (length byte-line) :element-type 'base-char)))
    (declare (type (simple-array base-char) char-line))
    (loop for i from 0 below (length byte-line)
          do (setf (aref char-line i)
                   (code-char (aref byte-line i))))
    char-line))

(defun read-byte-line (bb)
  "Reads bytes from byte-buffer BB up to the next newline or end of
stream, returning them as an array. The newline is not
included. Returns two values - the byte array and either T or NIL to
indicate whether a terminating newline was found."
  (multiple-value-bind (chunks has-newline)
      (read-chunks bb)
    (cond ((null chunks)
           (values nil nil))
          ((= 1 (length chunks))
           (values (first chunks) has-newline))
          (t
           (values (concatenate-chunks chunks) has-newline)))))

(defun is-empty-p (bb)
  "Returns TRUE if the byte-buffer BB is empty."
  (= (byte-buffer-offset bb) (byte-buffer-num-bytes bb)))

(defun fill-buffer (bb)
  "Fills the byte-buffer BB from the stream, returning the number of
bytes read."
  (setf (byte-buffer-offset bb) 0)
  (setf (byte-buffer-num-bytes bb)
        (read-sequence (byte-buffer-bytes bb) (byte-buffer-stream bb))))

(defun read-chunks (bb)
  "Reads chunks of bytes from byte-buffer BB, up to the next newline
or end of stream, returning them in a list. The newline is not
included. Returns two values - a list of chunks and either T or NIL to
indicate whether a terminating newline was found. When the stream
underlying the buffer is exhausted the list of chunks will be empty."
  (declare (optimize (speed 3) (safety 1) (debug 0)))
  (let ((nl-position (position (byte-buffer-nl-code bb) (byte-buffer-bytes bb)
                               :start (byte-buffer-offset bb)
                               :end (byte-buffer-num-bytes bb))))
    (cond ((and nl-position (plusp nl-position))
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
               (fill-buffer bb))
             (values (list chunk) t)))
          ((and nl-position (zerop nl-position))
           ;; There is a newline in the buffer at the zeroth
           ;; position. Make an empty chunk (for sake of
           ;; consistency). Move the offset beyond the newline. Fill
           ;; the buffer if necessary.
           (let ((chunk (make-array 0 :element-type '(unsigned-byte 8))))
             (setf (byte-buffer-offset bb) (1+ nl-position))
             (when (is-empty-p bb) (fill-buffer bb))
             (values (list chunk) t)))
          ((zerop (byte-buffer-num-bytes bb))
           ;; The buffer is empty
           (values nil nil))
          (t
           ;; There is no newline in the buffer. Make a chunk to
           ;; contain the rest of the buffered bytes and copy into
           ;; it. Fill the buffer. Recursively call read chunks to
           ;; search for the next newline.
           (let ((chunk (make-array (- (byte-buffer-num-bytes bb)
                                       (byte-buffer-offset bb))
                                    :element-type '(unsigned-byte 8)))
                 (chunks nil)
                 (nl nil))
             (array-copy (byte-buffer-bytes bb) chunk
                         :source-start (byte-buffer-offset bb)
                         :source-end (1- (byte-buffer-num-bytes bb)))
             (fill-buffer bb)
             (multiple-value-setq (chunks nl)
               (read-chunks bb))
             (values (cons chunk chunks) nl))))))

(defun concatenate-chunks (chunks)
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
