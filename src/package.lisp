
(defpackage #:cl-io-utilities
  (:use #:common-lisp)
  (:nicknames #:iou)
  (:documentation "IO utilities.")
  (:export
   ;; Conditions
   #:general-parse-error
   #:malformed-record-error
   ;; Classes
   #:line-buffer
   #:byte-line-buffer
   ;; Generics
   #:pull-line
   #:push-line
   #:more-lines-p
   #:find-line
   #:text-of
   ;; Functions
   #:make-line-buffer
   #:make-byte-line-buffer
   #:parse-float))
