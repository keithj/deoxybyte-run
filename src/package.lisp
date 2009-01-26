;;;
;;; Copyright (C) 2007-2009 Keith James. All rights reserved.
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

(defpackage #:cl-io-utilities
  (:use #:common-lisp #:cl-gp-utilities #:trivial-gray-streams)
  (:nicknames #:iou)
  (:documentation "IO utilities.")
  (:shadow #:type-of)
  (:export
   ;; Specials
   #:*empty-field*

   ;; Conditions
   #:io-error
   #:io-warning
   #:cli-error
   #:cli-warning
   #:unknown-command
   #:missing-required-option
   #:incompatible-argument
   #:unmatched-option
   #:unknown-option

   #:general-parse-error
   #:malformed-record-error
   #:malformed-field-error
   #:record-validation-error
   #:record-field-error

   ;; Classes
   #:line-input-stream
   #:character-line-input-stream
   #:binary-line-input-stream
   #:stream-filter-mixin

   #:external-program
   #:rsh
   
   ;; Generics
   #:push-line
   #:more-lines-p
   #:find-line
   #:text-of
   #:test-of
   #:record-of
   #:field-of

   #:program-of
   #:args-of
   #:process-of
   #:input-of
   #:output-of
   #:error-of
   #:wait-for
   #:status-of
   #:exit-code-of
   #:close-process
   #:kill-process

   #:host-of
   
   ;; Functions
   #:make-line-input-stream
   #:parse-float
   #:default-integer-parser
   #:default-float-parser

   #:parse-command-line
   #:print-cli-help
   #:cli-option
   #:print-option-help
   #:cli-opt-key
   #:cli-opt-name
   #:cli-opt-required-p
   #:cli-opt-documentation
   #:cli-arg-required-p
   #:cli-arg-type
   #:cli-arg-parser
   #:cli-arg-value
   #:quit-lisp
   
   #:make-tmp-pathname
   #:make-pathname-gen
   #:make-pathname-ext

   #:external-merge-sort

   #:rsh-exec
   
   ;; Macros
   #:define-line-parser
   #:with-li-stream
   #:with-ascii-li-stream
   #:with-cli
   #:with-argv
   #:with-backtrace))
