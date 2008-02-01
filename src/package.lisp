
(defpackage #:cl-io-utilities
  (:use #:common-lisp #:trivial-gray-streams)
  (:nicknames #:iou)
  (:documentation "IO utilities.")
  (:export
   ;; Conditions
   #:general-parse-error
   #:malformed-record-error
   ;; Classes
   #:line-input-stream
   #:character-line-input-stream
   #:binary-line-input-stream
   ;; Generics
   #:push-line
   #:more-lines-p
   #:find-line
   #:text-of
   ;; Functions
   #:make-line-input-stream
   #:parse-float))
