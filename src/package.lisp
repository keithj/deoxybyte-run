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
   #:*default-remote-host*
   #:*remote-pathname-defaults*

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
   #:non-zero-exit-error

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
   #:ssh-session
   
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
   #:user-of
   #:remote-shell-of
   #:remote-environment-of
   #:close-session
   #:open-session-p
   #:remote-command
   #:get-env
   #:set-env
   #:get-directory
   #:set-directory
   #:list-directory
   #:file-exists-p
   #:make-directory
   #:delete-directory-and-files
   
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

   #:absolute-pathname-p
   #:relative-pathname-p
   #:parse-file
   #:parse-directory
   #:ensure-file
   #:make-tmp-pathname
   #:make-pathname-gen
   #:make-pathname-ext

   #:external-merge-sort

   #:rsh-exec
   #:merge-remote-pathnames
   #:rsh-list-directory
   #:rsh-file-exists-p
   #:rsh-directory-exists-p
   #:rsh-files-exist-p
   #:rsh-directories-exist-p
   #:rsh-make-directory
   #:rsh-ensure-directories-exist

   #:open-session
   
   ;; Macros
   #:define-line-parser
   #:with-li-stream
   #:with-ascii-li-stream
   #:with-cli
   #:with-argv
   #:with-backtrace))
