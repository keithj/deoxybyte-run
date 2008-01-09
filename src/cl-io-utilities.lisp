
(in-package :cl-io-utilities)

(defconstant +byte-buffer-size+ 8192)

(deftype byte-buffer ()
  `(simple-array (unsigned-byte 8) (,+byte-buffer-size+)))

(deftype byte-buffer-index ()
  `(integer 0 ,+byte-buffer-size+))


;;; parse conditions

(define-condition general-parse-error (error)
  ((text :initform nil
         :initarg :text
         :reader text-of
         :documentation "Error message text."))
  (:report (lambda (condition stream)
             (format stream "general parse error~@[: ~a~]."
                     (text-of condition)))))

(define-condition malformed-record-error (general-parse-error)
  ()
  (:report (lambda (condition stream)
             (format stream "malformed record error~@[: ~a~]"
                     (text-of condition)))))

;;; line-buffer classes

(defclass line-buffer ()
  ((stream :initarg :stream
           :initform nil
           :reader stream-of
           :documentation "The underlying stream from which lines of
bytes are read, via the buffer.")
   (pushback :initform nil
             :accessor pushback-of
             :documentation "A list of lines that have been pushed
back into the reader to be read again."))
  (:documentation "Allows buffered reading of lines of characters from
a stream."))

(defclass byte-line-buffer (line-buffer)
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
  (:documentation "Allows buffered reading of lines of bytes from a
stream."))


;;; line-buffer generic functions

(defgeneric pull-line (line-buffer)
  (:documentation "Reads up to the next newline from LINE-BUFFER, or
end of stream, returning an array. The newline is not
included. Returns two values - the array and either T or NIL to
indicate whether a terminating newline was missing."))

(defgeneric push-line (line-buffer line)
  (:documentation "Pushes LINE back into LINE-BUFFER."))

(defgeneric more-lines-p (line-buffer)
  (:documentation "Returns T if LINE-BUFFER contains unread data."))

(defgeneric find-line (line-buffer test &optional max-lines)
  (:documentation "Iterates through lines pulled from LINE-BUFFER
until a line matching predicate TEST is found or until a number of
lines equal to MAX-LINES have been examined."))

(defgeneric read-chunks (byte-line-buffer)
  (:documentation "Reads chunks of bytes up to the next newline or end
of stream, returning them in a list. The newline is not
included. Returns two values - a list of chunks and either NIL or T to
indicate whether a terminating newline was missing. When the stream
underlying the buffer is exhausted the list of chunks will be empty."))

(defgeneric buffer-empty-p (byte-line-buffer)
  (:documentation "Returns T if the internal byte buffer of
LINE-BUFFER is empty."))

(defgeneric fill-buffer (byte-line-buffer)
  (:documentation "Fills the byte buffer of LINE-BUFFER from its
stream, setting the num-bytes slot to the number of bytes actually
read."))


;;; line-buffer constructor

(defun make-line-buffer (stream)
  "Returns a new LINE-BUFFER (or BYTE-LINE-BUFFER) wrapping
STREAM. The element type of STREAM must be either CHARACTER
or (UNSIGNED-BYTE 8)."
  (unless (and (streamp stream)
               (input-stream-p stream)
               (open-stream-p stream))
    (error "invalid STREAM argument ~a; expected an open input-stream"
           stream))
  (let ((elt-type (stream-element-type stream)))
    (cond ((subtypep elt-type 'character)
           (make-instance 'line-buffer :stream stream))
          ((equal '(unsigned-byte 8) elt-type)
           (make-instance 'byte-line-buffer :stream stream
                          :nl-code (char-code #\Newline)
                          :buffer (make-array +byte-buffer-size+
                                              :element-type '(unsigned-byte 8)
                                              :initial-element 0)))
          (t
           (error "invalid element type ~a from stream ~a"
                  elt-type stream)))))


;;; line-buffer methods

(defmethod initialize-instance :after ((obj byte-line-buffer) &key)
  (fill-buffer obj))

(defmethod pull-line ((obj line-buffer))
  (if (null (pushback-of obj))
      (multiple-value-bind (line missing-newline-p)
          (read-line (stream-of obj) nil nil)
        (values line missing-newline-p))
    (pop (pushback-of obj))))

(defmethod push-line ((obj line-buffer) (line string))
  (push line (pushback-of obj)))

(defmethod more-lines-p ((obj line-buffer))
  (or (pushback-of obj)
      (peek-char nil (stream-of obj) nil nil)))

(defmethod find-line ((obj line-buffer) test &optional max-lines)
  (do* ((line (pull-line obj) (pull-line obj))
        (matching-line-p (and line (funcall test line))
                         (and line (funcall test line)))
        (line-count 1 (1+ line-count)))
       ((or (null line)
            matching-line-p
            (and (not (null max-lines))
                 (= line-count max-lines)))
        (values line matching-line-p line-count))))


;;; byte-line-buffer methods

(defmethod pull-line ((obj byte-line-buffer))
  (if (null (pushback-of obj))
      (multiple-value-bind (chunks has-newline-p)
          (read-chunks obj)
        (cond ((null chunks)
               (values nil t))
              ((zerop (length (first chunks)))
               (first chunks))
              ((= 1 (length chunks))
               (values (first chunks) has-newline-p))
              (t
               (values (concatenate-chunks chunks) has-newline-p))))
    (pop (pushback-of obj))))

(defmethod push-line ((obj byte-line-buffer) (line vector))
  (push line (pushback-of obj)))

(defmethod more-lines-p ((obj byte-line-buffer))
  (or (pushback-of obj)
      (not (zerop (num-bytes-of obj)))))

(defmethod buffer-empty-p ((obj byte-line-buffer))
  (= (offset-of obj) (num-bytes-of obj)))

(defmethod fill-buffer ((obj byte-line-buffer))
  (setf (offset-of obj) 0
        (num-bytes-of obj) (read-sequence (buffer-of obj)
                                          (stream-of obj))))

(defmethod read-chunks ((obj byte-line-buffer))
  (declare (optimize (speed 3) (debug 0)))
  (let ((offset (offset-of obj))
        (num-bytes (num-bytes-of obj))
        (buffer (buffer-of obj)))
    (declare (type byte-buffer buffer)
             (type byte-buffer-index offset num-bytes))
    (let ((nl-position (position (nl-code-of obj) buffer
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
               (setf (offset-of obj) (1+ nl-position))
               (when (buffer-empty-p obj)
                 (fill-buffer obj))
               (values (list chunk) nil)))
            ((and nl-position
                  (zerop (- nl-position offset)))
             ;; There is a newline in the buffer at the zeroth
             ;; position. Make an empty chunk (for sake of
             ;; consistency). Move the offset beyond the newline. Fill
             ;; the buffer if necessary.
             (let ((chunk (make-array 0 :element-type '(unsigned-byte 8))))
               (setf (offset-of obj) (1+ nl-position))
               (when (buffer-empty-p obj)
                 (fill-buffer obj))
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
                   (missing-nl t))
               (gpu:copy-array buffer offset (1- num-bytes)
                               chunk 0)
               (fill-buffer obj)
               (multiple-value-setq (chunks missing-nl)
                 (read-chunks obj))
               (values (cons chunk chunks) missing-nl)))))))

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
