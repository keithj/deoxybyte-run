
(defpackage #:cl-io-utilities
  (:use #:common-lisp #:cl-gp-utilities)
  (:nicknames #:iou)
  (:documentation "IO utilities.")
  (:export
   ;; Conditions

   ;; Classes
   #:line-buffer
   ;; Generics
   #:pull-line
   #:push-line
   ;; Functions
   #:make-line-buffer))
