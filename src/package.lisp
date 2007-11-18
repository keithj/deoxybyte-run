
(defpackage #:cl-io-utilities
  (:use #:common-lisp)
  (:nicknames #:iou)
  (:documentation "IO utilities.")
  (:export
   ;; Conditions

   ;; Classes
   #:buffered-reader
   ;; Generics
   #:buf-read-line
   ;; Functions
   #:make-line-reader
   #:convert-to-char))
