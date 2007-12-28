
(defpackage #:cl-io-utilities
  (:use #:common-lisp)
  (:nicknames #:iou)
  (:documentation "IO utilities.")
  (:export
   ;; Conditions

   ;; Classes
   #:line-buffer
   #:byte-line-buffer
   ;; Generics
   #:pull-line
   #:push-line
   #:more-lines-p
   #:find-line
   ;; Functions
   #:make-line-buffer
   #:make-byte-line-buffer))
