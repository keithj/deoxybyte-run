;;;
;;; Copyright (C) 2009 Keith James. All rights reserved.
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

(in-package :cl-io-utilities)

(defparameter *sh-executable* (pathname "/bin/sh")
  "The remote pathname of the sh executable.")
(defparameter *bash-executable* (pathname "/bin/bash")
  "The remote pathname of the bash executable.")
(defparameter *tcsh-executable* (pathname "/bin/tcsh")
  "The remote pathname of the tcsh executable.")

(defun remote-shell-executable (name)
  "Returns a pathname of the remote shell executable for symbol
NAME. NAME may be one of :sh :bash :csh or :tcsh ."
  (ecase name
    (:sh *sh-executable*)
    (:bash *bash-executable*)
    ((:csh :tcsh) *tcsh-executable*)))

(defclass ssh-session (external-program)
  ((host :initform "localhost"
         :initarg :host
         :reader host-of
         :documentation "The remote host on which the session is
         executed.")
   (user :initform nil
         :initarg :user
         :reader user-of
         :documentation "The user for the ssh session.")
   (remote-shell :initform :sh
                 :initarg :remote-shell
                 :reader remote-shell-of
                 :documentation "The shell to be started on the remote
                 host. If NIL, the default login shell for the user is
                 used.")
   (remote-environment :initform nil
                       :initarg :remote-environment
                       :reader remote-environment-of
                       :documentation "An alist of environment
                       variables and values to be set for the session
                       on the remote host. Keys and values may be
                       symbols and/or strings."))
  (:documentation "An instance of this class represents an ssh
session."))

(defgeneric open-session-p (session)
  (:documentation "Returns T if SESSION is open, or NIL otherwise."))

(defgeneric remote-command (session command
                           &key non-zero-error exit-code void-command)
  (:documentation "Executes shell COMMAND in SESSION.

Arguments:

- session (ssh-session): The {defclass ssh-session} .
- command (string): The shell command.

Key:

- non-zero-error (boolean): If T a {define-condition non-zero-exit-error}
condition will be raised should the command exit with a non-sero exit code.
- exit-code (boolean): If T an attempt will be made to determine the
exit code of the command.
- void-command (boolean): If T no attempt will be made to read a
  return value from the command.

Returns:

- A list of strings returned by the command or NIL.
- An integer exit code, or NIL."))

(defgeneric get-env (session variable)
  (:documentation "Returns the value of VARIABLE in the shell
  environment of SESSION."))

(defgeneric set-env (session variable value)
  (:documentation "Sets VARIABLE in the shell environment of SESSION
  to VALUE. Returns the new value."))

(defgeneric close-session (session)
  (:documentation "Closes SESSION by sending an exit command and then
  closing in the streams to and from the ssh process."))

(defun open-session (&key (host "localhost") user (remote-shell :sh)
                     remote-environment (timeout 10) keep-alive)
  "Opens a new SSH session on HOST and returns an {defclass ssh-session}
object.

Key:

- host (string): SSH host name (defaults to \"localhost\".
- user (string): The SSH user name (defaults to the current user).
- remote-shell (symbol): The shell to be used on the remote host.
- remote-environment (alist): An association list used to set
  environment variables on the remote host at the start of the
  session. Keys and values may be strings or symbols.
- timeout (integer): The SSH connection opening timeout in
  seconds (defaults to 10). Setting this to NIL disables the timeout.
- keep-alive (integer): The interval at which to send keep-alive
  signals (defaults to NIL). Setting this to NIL disables keep-alive.

Returns:
- An {defclass ssh-session} object."
  (make-instance 'ssh-session :program "ssh"
                 :args (concatenate
                        'list '("-o" "BatchMode=yes")
                        (when timeout
                          (list "-o" (format nil "ConnectTimeout=~d"
                                             timeout)))
                        (when keep-alive
                          (list "-o" (format nil "ServerAliveInterval=~d"
                                             keep-alive)))
                        (when user
                          (list "-l" user)) (list host))
                 :user user
                 :host host
                 :remote-shell remote-shell
                 :remote-environment remote-environment
                 :input :stream :output :stream :error :output
                 :pty nil
                 :search t
                 :wait nil))

(defmethod initialize-instance :after ((session ssh-session) &key)
  (with-accessors ((output output-of) (shell remote-shell-of)
                   (environment remote-environment-of))
      session
    (when shell
      (remote-command session
                      (format nil "exec ~a" (remote-shell-executable shell))
                      :void-command t))
    (loop
       for (var . val) in environment
       do (set-env session var val))))

(defmethod print-object ((session ssh-session) stream)
  (with-accessors ((host host-of) (user user-of) (shell remote-shell-of))
      session
    (format stream "#<SSH-SESSION host: ~a user: ~a shell: ~a>"
            host user shell)))

(defmethod open-session-p ((session ssh-session))
  (and (eql :running (status-of session))
       (open-stream-p (input-of session))
       (open-stream-p (output-of session))))

(defmethod remote-command ((session ssh-session) (command string)
                           &key (non-zero-error t) (exit-code t)
                           void-command)
  (with-accessors ((input input-of) (output output-of))
      session
    (send-command command input)
    (let ((response (unless void-command
                      (read-response output))))
      (let ((code (when exit-code
                    (handler-case
                        (progn
                          (send-command "echo $?" input)
                          (parse-integer (first (read-response output))))
                      (parse-error ()
                        (error 'non-zero-exit-error :program "echo $?"))))))
        (cond ((and exit-code (zerop code))
               (values response code))
              ((not exit-code)
               (values response nil))
              (non-zero-error
               (error 'non-zero-exit-error :program command
                      :exit-code code))
              (t
               (values response code)))))))

(defmethod get-env ((session ssh-session) variable)
  (let ((var (etypecase variable
               (symbol (symbol-name variable))
               (string variable))))
    (remote-command session (format nil "echo $~a" var))))

(defmethod set-env ((session ssh-session) variable value)
  (let ((var (etypecase variable
               (symbol (symbol-name variable))
               (string variable)))
        (val (etypecase value
               (symbol (symbol-name value))
               (string value))))
    (let ((cmd (ecase (remote-shell-of session)
                 ((:sh :bash) (format nil "export ~a=~a ; echo $~a"
                                      var val var))
                 ((:csh :tcsh) (format nil "setenv ~a ~a ; echo $~a"
                                       var val var)))))
      (remote-command session cmd))))

(defmethod close-session ((session ssh-session))
  (remote-command session "exit" :non-zero-error nil :void-command t
                  :exit-code nil)
  session)

(defun send-command (command stream)
  "Writes string COMMAND to STREAM and flushes STREAM."
  (write-line command stream)
  (finish-output stream))

(defun read-response (stream)
  "Returns a list of strings that are lines collected from STREAM."
  (loop
     for line = (read-response-line stream)
     then (read-response-line stream t)
     while line
     collect line))

(defun read-response-line (stream &optional listen)
  "Returns a new line read from STREAM. When LISTEN is T, a read
operation will only be attempted if calling the cl:listen function on
the stream returns T. This is to prevent a blocking read."
  (when (or (not listen)
            (and listen (listen stream)))
    (loop
       for char = (read-char stream t)
       until (char= #\Newline char)
       collect char into line
       finally (return (when line
                         (make-array (length line) :element-type 'character
                                     :initial-contents line))))))
