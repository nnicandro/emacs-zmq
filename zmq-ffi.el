;;; zmq-ffi.el --- Emacs bindings to ZMQ

;; Copyright (C) 2018 Nathaniel Nicandro

;; Author: Nathaniel Nicandro <nathanielnicandro@gmail.com>
;; Created: 19 Jan 2018
;; Version: 0.0.1
;; X-URL: https://github.com/nathan/emacs-zmq

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;

;;; Code:

(require 'cl-lib)
(require 'ffi)
(require 'zmq-constants)
(define-ffi-library libzmq "libzmq")

(defvar zmq--live-sockets nil
  "A list of sockets which have not yet been closed.
These sockets will be properly cleaned up if Emacs exits and they
are still not closed. See `zmq-cleanup-on-exit'.")

;;; Types

(define-ffi-array zmq--msg-t :char 64)

(define-ffi-struct zmq--poller-event-t
  (socket :type :pointer)
  (fd :type :int)
  (user-data :type :pointer)
  (events :type :short))

(define-ffi-struct zmq--pollitem-t
  (socket :type :pointer)
  (fd :type :int)
  (events :type :short)
  (revents :type :short))

(cl-defstruct (zmq-pollitem
               (:constructor nil)
               (:constructor zmq-pollitem))
  (socket nil)
  (fd -1)
  (events 0))

;;; FFI wrapper

(eval-and-compile
  (defun zmq--ffi-normalize-arg-types (args arg-types)
    "Handle ARG-TYPES specific to ZMQ.

ARGS should be a list of argument names with their types
specified in the vector ARG-TYPES.

This function destructively modifies any elements of ARG-TYPES
with the special types `:socket', `:context', `:poller',
`:message', or `:string'. If any of the elements of the ARG-TYPES
array has one of these values it sets it to `:pointer' and
collects either a string binding for if the special value was
`:string' or an argument unwrapping form for the other special
values.

If an element of ARG-TYPES is `:socket', `:context', `:poller',
or `:message' then the corresponding argument in ARGS is assumed
to be a `zmq-socket', `zmq-context', `zmq-poller', or
`zmq-message' object. Since these objects are not suitable to be
passed to a C function, they must first be unwrapped into their C
pointer equivalent. Therefore whenever one of the special types
other than `:string' is encountered in ARG-TYPES, an argument
unwrapping form will be generated:

    (setq ARG (if (eq ARG nil) (ffi-null-pointer)
                (zmq-<special type>--ptr ARG))

The return value is cons cell (STRING-BINDINGS . ARG-UNWRAPPERS)
where STRING-BINDINGS is either nil or a list of string bindings
that can be passed as the first argument to `with-ffi-strings'.
ARG-UNWRAPPERS is either nil or a list of the argument unwrapping
forms that where generated as described above."
    (cl-loop
     with special-types = '((:socket . "zmq-socket")
                            (:context . "zmq-context")
                            (:poller . "zmq-poller")
                            (:message . "zmq-message"))
     with st = nil
     for i from 0 to (1- (length arg-types))
     ;; Handle `:string' arguments
     when (eq (aref arg-types i) :string)
     do (aset arg-types i :pointer)
     and collect (let ((arg (nth i args)))
                   (list arg arg))
     into string-bindings
     ;; Handle the other special types besides `:string'
     when (setq st (alist-get (aref arg-types i) special-types))
     do (aset arg-types i :pointer)
     and collect
     (let ((arg (nth i args)))
       ;; NOTE Calling cl functions already checks the type before
       ;; accessing any struct fields.
       `(setq ,arg (if (eq ,arg nil) (ffi-null-pointer)
                     (,(intern (concat st "--ptr")) ,arg))))
     into arg-unwrappers
     finally return (cons string-bindings arg-unwrappers))))

(defmacro zmq--ffi-wrapper (c-name return-type arg-types &optional noerror)
  "Generate an FFI for a ZMQ function and wrap it with error handling.
C-NAME is the C function name as a string and without the zmq_
prefix. RETURN-TYPE is the return type as expected by
`define-ffi-function'. ARG-TYPES can either be a vector of types,
also in the sense of `define-ffi-function', or it can be a list
of lists

    ((arg1 :type1) (arg2 :type2) ...)

where (arg1, arg2, ...) will be the argument names of the wrapped
function and (:type1, :type2, ...) are the types for each
argument in the FFI for the C function.

If any of the types in ARG-TYPES are `:string', then it indicates
that the corresponding argument is intended to be a string passed
to the C function. If any of the ARG-TYPES are the special types
`:message', `:socket', `:context', or `:poller', then it
indicates that the corresponding argument is a `zmq-message',
`zmq-socket', `zmq-context', or `zmq-poller' respectively.
Whenever any of these types appear, the appropriate type checking
will happen when calling the wrapped function.

This macro defines the FFI function as zmq--<name>-1 and the
wrapped function with error handling and type checking as
zmq--<name> where <name> is C-NAME with all '_' characters
replaced with '-'. To turn off error handling for the generated
function, pass a non-nil value to the NOERROR argument."
  (declare (debug t) (indent 1))
  (let* ((fname (subst-char-in-string ?_ ?- c-name))
         (c-name (concat "zmq_" c-name))
         (ffi-name (intern (concat "zmq--" fname "-1")))
         (wrapped-name (intern (concat "zmq--" fname)))
         (args (if (listp arg-types)
                   ;; When list, assume it is of the form ((arg1 :type1) ...),
                   ;; collect the argument names and set ARG-TYPES to a vector.
                   (cl-loop
                    for (name type) in arg-types
                    collect name and collect type into types
                    finally do (setq arg-types (apply #'vector types)))
                 ;; Otherwise assume ARG-TYPES is already a vector and collect
                 ;; generated argument names.
                 (cl-loop repeat (length arg-types)
                          collect (cl-gensym))))
         (body `(let ((ret (,ffi-name ,@args)))
                  ;; Depending on the RETURN-TYPE, check if an error is
                  ;; set.
                  ,(unless noerror
                     `(when ,(cl-case return-type
                               (:pointer '(ffi-pointer-null-p ret))
                               (:int '(= ret -1))
                               ;; Just return without checking for errors
                               (t nil))
                        (zmq-error-handler)))
                  ret)))
    (cl-destructuring-bind (string-bindings . arg-unwrappers)
        (zmq--ffi-normalize-arg-types args arg-types)
      `(progn
         (define-ffi-function ,ffi-name ,c-name ,return-type ,arg-types libzmq)
         (defun ,wrapped-name ,args
           ,@(when arg-unwrappers
               arg-unwrappers)
           ,(if string-bindings
                `(with-ffi-strings ,string-bindings
                   ,body)
              body))))))

;;; Memory handling functions

(defun zmq--get-bytes (buf size)
  "From BUF get SIZE bytes and return a `unibyte-string'."
  (let (data)
    (while (>= (setq size (1- size)) 0)
      (setq data (cons (ffi--mem-ref (ffi-pointer+ buf size) :char) data)))
    (apply #'unibyte-string data)))

(defun zmq--set-bytes (buf data)
  "Set the contents of BUF to DATA.

DATA must be a string and BUF must be a pointer to memory which
can hold at least (length DATA) of bytes."
  (dotimes (i (length data))
    (ffi--mem-set (ffi-pointer+ buf i) :char (aref data i))))

;;; Utility functions

(eval-and-compile
  (zmq--ffi-wrapper "has" :int ((capability :string)) noerror)
  (zmq--ffi-wrapper "version" :void ((major :pointer) (minor :pointer) (patch :pointer)) noerror)

  (defun zmq-has (capability)
    "Does ZMQ have CAPABILITY?"
    (= (zmq--has capability) 1))

  (defun zmq-version ()
    "Get the version of ZMQ."
    (with-ffi-temporaries ((major :int)
                           (minor :int)
                           (patch :int))
      (zmq--version major minor patch)
      (apply #'format "%d.%d.%d"
             (mapcar (lambda (x) (ffi--mem-ref x :int))
                (list major minor patch))))))

;;; Error handling

(define-ffi-function zmq-errno "zmq_errno" :int [] libzmq)
(define-ffi-function zmq-strerror "zmq_strerror" :pointer [:int] libzmq)

(define-error 'zmq-ERROR "An error occured in ZMQ" 'error)

(cl-loop
 for (errno . sym) in zmq-error-alist
 do (define-error sym (ffi-get-c-string (zmq-strerror errno)) 'zmq-ERROR))

(defun zmq-error-handler (&rest data)
  "Handle the error created by a call to a ZMQ API function.
Signal the proper error corresponding to `zmq-errno' by using
`zmq-error-alist'. DATA is any additional data which will be
passed as the second argument to `signal'."
  (let* ((errno (zmq-errno))
         (errsym (or (cdr (assoc errno zmq-error-alist))
                     'zmq-ERROR)))
    (signal errsym data)))

;;; Contexts

(cl-defstruct (zmq-context
               (:constructor nil)
               (:constructor
                zmq-context
                (&aux (-ptr (or (zmq--ctx-new)
                                (error "Context not created"))))))
  (-ptr nil :read-only t))

;; See the `zmq-context' type
(zmq--ffi-wrapper "ctx_new" :pointer () noerror)
(zmq--ffi-wrapper "ctx_set" :int ((context :context) (option :int) (value :int)))
(zmq--ffi-wrapper "ctx_get" :int ((context :context) (option :int)))
(zmq--ffi-wrapper "ctx_term" :int ((context :context)))
(zmq--ffi-wrapper "ctx_shutdown" :int ((context :context)))

(defun zmq-context-set (context option value)
  "Set a CONTEXT OPTION to VALUE."
  (when (and (booleanp value)
             (member option `(,zmq-BLOCKY ,zmq-IPV6)))
    (setq value (if value 1 0)))
  (zmq--ctx-set context option value))

(defun zmq-context-get (context option)
  "Get a CONTEXT OPTION."
  (let ((value (zmq--ctx-get context option)))
    (if (member option `(,zmq-BLOCKY ,zmq-IPV6))
        (= value 1)
      value)))

(defun zmq-terminate-context (context)
  "Terminate CONTEXT."
  (while (condition-case err
             (prog1 nil
               (zmq--ctx-term context))
           (zmq-EINTR t))))

(defalias 'zmq-shutdown-context #'zmq--ctx-shutdown
  "Shutdown CONTEXT.")

;;; Encryption

(zmq--ffi-wrapper "z85_decode" :pointer ((decoded :pointer) (str :string)) noerror)
(zmq--ffi-wrapper "z85_encode" :pointer ((encoded :pointer) (str :string) (len :size_t)) noerror)
(zmq--ffi-wrapper "curve_keypair" :int ((public :pointer) (secret :pointer)))
(zmq--ffi-wrapper "curve_public" :int ((public :pointer) (secret :string)))

(defun zmq-z85-decode (key)
  "Decode a z85 encoded KEY."
  (unless (= (mod (length key) 5) 0)
    (signal 'args-out-of-range '("Length not a multiple of 5")))
  (let ((size (ceiling (* 0.8 (length key)))))
    (with-ffi-temporary (decoded size)
      (when (ffi-pointer= decoded (zmq--z85-decode decoded key))
        (zmq--get-bytes decoded size)))))

(defun zmq-z85-encode (data)
  "Encode DATA using the z85 encoding."
  (unless (= (mod (length data) 4) 0)
    (signal 'args-out-of-range '("Length not a multiple of 4")))
  (let ((size (+ (ceiling (* 1.25 (length data))) 1)))
    (with-ffi-temporary (encoded size)
      (when (ffi-pointer= encoded (zmq--z85-encode encoded data (length data)))
        (ffi-get-c-string encoded)))))

(defun zmq-curve-keypair ()
  "Return a (PUBLIC-KEY . SECRET-KEY) pair."
  (unless (zmq-has "curve")
    (error "ZMQ not built with CURVE security"))
  (with-ffi-temporaries ((public-key 41)
                         (secret-key 41))
    (zmq--curve-keypair public-key secret-key)
    (cons (ffi-get-c-string public-key)
          (ffi-get-c-string secret-key))))

(defun zmq-curve-public (secret-key)
  "Return the corresponding public key of SECRET-KEY."
  (unless (zmq-has "curve")
    (error "ZMQ not built with CURVE security"))
  (with-ffi-temporary (public-key 41)
    (zmq--curve-public public-key secret-key)
    (ffi-get-c-string public-key)))

;;; Messages

(cl-defstruct (zmq-message
               (:constructor
                zmq-message
                (&optional
                 data &aux
                 (-ptr (let ((msg (ffi-allocate zmq--msg-t)))
                         (cond
                          ((or (stringp data) (vectorp data) (null data))
                           (if (= (length data) 0) (zmq--msg-init msg)
                             (zmq--msg-init-size msg (length data))
                             (zmq--set-bytes (zmq--msg-data msg) data)))
                          (t (ffi-free msg)
                             (signal
                              'wrong-type-argument
                              (list (format "Can't initialize message with data (%s)"
                                            data)))))
                         msg)))))
  (-ptr nil :read-only t))

(zmq--ffi-wrapper "msg_init" :int ((messagep :pointer)))
(zmq--ffi-wrapper "msg_init_size" :int ((messagep :pointer) (size :size_t)))
;; NOTE: Closures don't work in the ffi interface, so msg_init_data is not
;; wrapped.

(defun zmq-init-message (message &optional data)
  "Initialize a MESSAGE with DATA.

DATA can be either a string, a vector, or nil.

If DATA is a string it should not contain any multi-byte
characters. If DATA is a vector it should be a vector of integers
within the range 0-255, i.e. each element is a byte. In these two
cases, MESSAGE is initialized with the contents of DATA. When
DATA is nil, initialize an empty message."
  (let ((ptr (zmq-message--ptr message)))
    (if (null data) (zmq--msg-init ptr)
      (zmq--msg-init-size ptr (length data))
      (zmq--set-bytes (zmq--msg-data ptr) data))))

(zmq--ffi-wrapper "msg_recv" :int ((message :message) (sock :socket) (flags :int)))
(zmq--ffi-wrapper "msg_send" :int ((message :message) (sock :socket) (flags :int)))

(defun zmq-recv-message (message sock &optional flags)
  "Receive a MESSAGE from SOCK with additional FLAGS.

MESSAGE should be an initialized message."
  (zmq--msg-recv message sock (or flags 0)))

(defun zmq-send-message (message sock &optional flags)
  "Send a MESSAGE on SOCK with additional FLAGS."
  (zmq--msg-send message sock (or flags 0)))

(zmq--ffi-wrapper "msg_move" :int ((dest :message) (src :message)))
(zmq--ffi-wrapper "msg_copy" :int ((dest :message) (src :message)))
(zmq--ffi-wrapper "msg_close" :int ((message :message)))

(defalias 'zmq-move-message 'zmq--msg-move
  "Move a message from SRC to DEST.")

(defun zmq-copy-message (message)
  "Copy MESSAGE."
  (let ((dest (zmq-message)))
    (condition-case err
        (prog1 dest
          (zmq--msg-copy dest message))
      (error (zmq-close-message dest)
             (signal (car err) (cdr err))))))

(defun zmq-close-message (message)
  "Close a MESSAGE."
  (unwind-protect
      (zmq--msg-close message)
    (ffi-free (zmq-message--ptr message))))

;; Used in `zmq-message' struct initialization
(zmq--ffi-wrapper "msg_data" :pointer ((messagep :pointer)) noerror)
(zmq--ffi-wrapper "msg_size" :size_t ((message :message)))
(zmq--ffi-wrapper "msg_more" :int ((message :message)))

(defun zmq-message-data (message)
  "Get the data of MESSAGE."
  (let ((data (zmq--msg-data (zmq-message--ptr message))))
    (when data
      (zmq--get-bytes data (zmq--msg-size message)))))

(defalias 'zmq-message-size 'zmq--msg-size
  "Get the size of MESSAGE.")

(defun zmq-message-more-p (message)
  "Does MESSAGE have more parts?"
  (= (zmq--msg-more message) 1))

(defconst zmq-message-properties '((:socket-type . "Socket-Type")
                                   (:identity . "Identity")
                                   (:resource . "Resource")
                                   (:peer-address . "Peer-Address")
                                   (:user-id . "User-Id"))
  "Alist mapping keywords to their corresponding message property.
A message's metadata property can be accessed through
`zmq-message-property'.")

(zmq--ffi-wrapper "msg_set" :int ((message :message) (property :int) (value :int)))
(zmq--ffi-wrapper "msg_get" :int ((message :message) (property :int)))
(zmq--ffi-wrapper "msg_gets" :pointer ((message :message) (property :string)))
(zmq--ffi-wrapper "msg_routing_id" :uint32 ((message :message)))
(zmq--ffi-wrapper "msg_set_routing_id" :int ((message :message) (id :int)))

(defun zmq-message-set (message property value)
  "Set a MESSAGE's PROPERTY to VALUE."
  (when (and (booleanp value)
             (= property zmq-MORE))
    (setq value (if value 1 0)))
  (zmq--msg-set message property value))

(defun zmq-message-get (message property)
  "Get a MESSAGE PROPERTY."
  (let ((value (zmq--msg-get message property)))
    (if (= property zmq-MORE)
        (= value 1)
      value)))

(defun zmq-message-property (message property)
  "Get a MESSAGE's metadata PROPERTY.

PROPERTY is a keyword and can only be one of those in
`zmq-message-properties'."
  (let ((prop (cdr (assoc property zmq-message-properties))))
    (unless prop
      (signal 'args-out-of-range
              (list (mapcar #'car zmq-message-properties) prop)))
    (decode-coding-string
     (ffi-get-c-string (zmq--msg-gets
                        message (encode-coding-string prop 'utf-8)))
     'utf-8)))

(defalias 'zmq-message-id 'zmq--msg-routing-id
  "Get the routing ID of MESSAGE.")

(defalias 'zmq-message-set-id 'zmq--msg-set-routing-id
  "Set the routing ID of MESSAGE.")

;;; Polling

(defun zmq--split-poll-events (events)
  "Split EVENTS into a list of polling events.
EVENTS should be a bitmask of polling events which will be
transformed into a list of polling events contained in EVENTS.
The returned list can contain any or all of the elements
zmq-POLLIN, zmq-POLLOUT, or zmq-POLLERR."
  (cl-loop
   for e in `(,zmq-POLLIN ,zmq-POLLOUT ,zmq-POLLERR)
   if (/= (logand e events) 0) collect e))

(zmq--ffi-wrapper "poll" :int ((items :pointer) (len :int) (timeout :long)))

(defun zmq-poll (items timeout)
  "Poll the list of `zmq-pollitem's in ITEMS until TIMEOUT ms.
If any events for ITEMS occur within TIMEOUT, return a list of
cons cells where each element has the form

    (SOCK-OR-FD . EVENTS)

where SOCK-OR-FD is either a `zmq-socket' or file descriptor for
the item being polled which received EVENTS. EVENTS is a list
containing the events that occured within TIMEOUT. You can test
for membership of an event like so

    (member zmq-POLLIN EVENTS)

Note that if TIMEOUT is -1, wait indefinately until an event arrives."
  (when (> (length items) 0)
    (let ((size (ffi--type-size zmq--pollitem-t))
          (found 0))
      (with-ffi-temporary (head (* (length items) size))
        ;; Construct zmq--pollitem-t objects
        (cl-loop
         with pointer = head
         for item in items do
         ;; socket
         (setf (zmq--pollitem-t-socket pointer)
               (if (zmq-pollitem-socket item)
                   (zmq-socket--ptr (zmq-pollitem-socket item))
                 (ffi-null-pointer)))
         ;; fd
         (setf (zmq--pollitem-t-fd pointer)
               (zmq-pollitem-fd item))
         ;; events
         (setf (zmq--pollitem-t-events pointer)
               (let ((events (zmq-pollitem-events item)))
                 (if (listp events) (apply #'logior events)
                   events)))
         ;; revents
         (setf (zmq--pollitem-t-revents pointer) 0)
         ;; increment pointer
         (setq pointer (ffi-pointer+ pointer size)))
        (setq found (zmq--poll head (length items) timeout))
        (when (> found 0)
          (cl-loop
           with pointer = head
           for item in items
           for revents = (zmq--pollitem-t-revents pointer)
           if (> revents 0) collect
           (cons (if (ffi-pointer-null-p (zmq--pollitem-t-socket pointer))
                     (zmq--pollitem-t-fd pointer)
                   (zmq-pollitem-socket item))
                 (zmq--split-poll-events revents))
           and do (setq pointer (ffi-pointer+ pointer size))))))))

(when (zmq-has "draft")
  (cl-defstruct (zmq-poller
                 (:constructor nil)
                 (:constructor
                  zmq-poller
                  (&aux (-ptr (or (zmq--poller-new)
                                  (error "Poller not created"))))))
    (-ptr nil :read-only t)
    (-socks-fds nil))

  ;; TODO: Handle windows machines
  ;; See `zmq-poller' type
  (zmq--ffi-wrapper "poller_new" :pointer ())
  (zmq--ffi-wrapper "poller_destroy" :int ((pollerp :pointer)))

  (defun zmq-destroy-poller (poller)
    "Destroy a POLLER."
    (let ((ptr (zmq-poller--ptr poller)))
      (with-ffi-temporary (pptr :pointer)
        (ffi--mem-set pptr :pointer (ffi-pointer+ ptr 0))
        (zmq--poller-destroy pptr))))

  (zmq--ffi-wrapper "poller_add" :int ((poller :poller) (sock :socket) (user-data :pointer) (events :short)))
  (zmq--ffi-wrapper "poller_add_fd" :int ((poller :poller) (fd :int) (user-data :pointer) (events :short)))
  (zmq--ffi-wrapper "poller_modify" :int ((poller :poller) (sock :socket) (events :short)))
  (zmq--ffi-wrapper "poller_modify_fd" :int ((poller :poller) (fd :int) (events :short)))
  (zmq--ffi-wrapper "poller_remove" :int ((poller :poller) (sock :socket)))
  (zmq--ffi-wrapper "poller_remove_fd" :int ((poller :poller) (fd :int)))

  (defun zmq-poller-add (poller sock-or-fd events &optional user-data)
    "Listen for EVENTS on SOCK-OR-FD using POLELR.

SOCK-OR-FD can either be a `zmq-socket' or a file descriptor.
EVENTS can either be a list of events (one of `zmq-POLLIN',
`zmq-POLLOUT', `zmq-POLLERR') or a bitwise-or of events. Optional
arguments USER-DATA is currently ignored."
    (let ((events (if (listp events) (apply #'logior events)
                    events)))
      (when (condition-case err
                (zmq-poller-modify poller sock-or-fd events)
              (zmq-EINVAL t))
        (setq user-data (or user-data (ffi-null-pointer)))
        (if (integerp sock-or-fd)
            (zmq--poller-add-fd poller sock-or-fd user-data events)
          (zmq--poller-add poller sock-or-fd user-data events))
        (setf (zmq-poller--socks-fds poller)
              (cons sock-or-fd (zmq-poller--socks-fds poller))))))

  (defun zmq-modify-poller (poller sock-or-fd events)
    "Modify the EVENTS of SOCK-OR-FD that POLLER listens for."
    (let ((events (if (listp events) (apply #'logior events)
                    events)))
      (if (integerp sock-or-fd)
          (zmq--poller-modify-fd poller sock-or-fd events)
        (zmq--poller-modify poller sock-or-fd events))))

  (defun zmq-poller-remove (poller sock-or-fd)
    "Remove SOCK-OR-FD from POLLER."
    (when (condition-case err
              (prog1 t
                (if (integerp sock-or-fd)
                    (zmq--poller-remove-fd poller sock-or-fd)
                  (zmq--poller-remove poller sock-or-fd)))
            (zmq-EINVAL nil))
      (setf (zmq-poller--socks-fds poller)
            (cl-remove sock-or-fd (zmq-poller--socks-fds poller)
                       :test (lambda (a b)
                               (or (and (zmq-socket-p a)
                                        (zmq-socket-p b)
                                        (ffi-pointer= (zmq-socket--ptr a)
                                                      (zmq-socket--ptr b)))
                                   (and (integerp a) (integerp b) (= a b))))))))

  (defun zmq-poller-register (poller sock-or-fd events)
    "Register the EVENTS of SOCK-OR-FD on POLLER."
    (zmq-poller-add poller sock-or-fd events))

  (defun zmq-poller-unregister (poller sock-or-fd)
    "Unregister SOCK-OR-FD from POLLER."
    (zmq-poller-remove poller sock-or-fd))

  (zmq--ffi-wrapper "poller_wait" :int ((poller :poller) (event :pointer) (timeout :long)))
  (zmq--ffi-wrapper "poller_wait_all" :int ((poller :poller) (events :pointer) (nevents :int) (timeout :long)))

  (defun zmq--poller-event-trigger (poller event)
    (let ((esock (zmq--poller-event-t-socket event)))
      (or (unless (ffi-pointer-null-p esock)
            (cl-loop
             for s in (zmq-poller--socks-fds poller)
             when (and (zmq-socket-p s) (ffi-pointer= (zmq-socket--ptr s) esock))
             return s))
          (zmq--poller-event-t-fd event))))

  (defun zmq-poller-wait (poller timeout)
    "Poll for an event with POLLER until TIMEOUT ms.

If an event occures before TIMEOUT ms, return a cons
cell (SOCK-OR-FD . EVENTS) where EVENTS is a list of events which
occured before TIMEOUT. Otherwise return nil. If TIMEOUT is -1,
wait forever until an event arrives."
    (with-ffi-temporaries ((e zmq--poller-event-t))
      (condition-case err
          (when (>= (zmq--poller-wait poller e timeout) 0)
            (cons (zmq--poller-event-trigger poller e)
                  (zmq--split-poll-events (zmq--poller-event-t-events e))))
        (zmq-ETIMEDOUT nil))))

  (defun zmq-poller-wait-all (poller nevents timeout)
    "Wait until TIMEOUT for NEVENTS on POLLER.

If between 1 and NEVENTS events occured within TIMEOUT (measured
in milliseconds) return a list of cons cells, each element having
the form (SOCK-OR-FD . EVENTS). EVENTS is a list of events which
occured on SOCK-OR-FD during the polling period. Note that the
length of the returned list may be less than NEVENTS if less than
NEVENTS events occurred within TIMEOUT. If TIMEOUT is -1, wait
forever."
    (let ((size (ffi--type-size zmq--poller-event-t)))
      (with-ffi-temporaries ((es (* size nevents)))
        (condition-case err
            (let ((found (zmq--poller-wait-all poller es nevents timeout))
                  (events nil)
                  (e nil))
              (while (>= (setq found (1- found)) 0)
                ;; TODO: What about user-data?
                (setq
                 e (ffi-pointer+ es (* found size))
                 events (cons (cons (zmq--poller-event-trigger poller e)
                                    (zmq--split-poll-events
                                     (zmq--poller-event-t-events e)))
                              events)))
              events)
          (zmq-ETIMEDOUT nil))))))

;;; Proxy

(zmq--ffi-wrapper "proxy_steerable" :int ((frontend :socket) (backend :socket) (capture :socket) (control :socket)))
(zmq--ffi-wrapper "proxy" :int ((frontend :socket) (backend :socket) (capture :socket)))

(defalias 'zmq-proxy-steerable #'zmq--proxy-steerable
  "Start the built-in ZMQ proxy with possible control flow.")

(defalias 'zmq-proxy #'zmq--proxy
  "Start the built-in ZMQ proxy.")

;;; Sockets

(cl-defstruct (zmq-socket
               (:constructor nil)
               (:constructor
                zmq-socket
                (ctx type &aux (-ptr (if (integerp type) (zmq--socket ctx type)
                                       (signal 'wrong-type-argument
                                               (list 'integerp type)))))))
  (-ptr nil :read-only t))

(defun zmq--live-sockets (sock)
  "Add SOCK to the `zmq--live-sockets' list.
When `zmq-close' is called on SOCK, it will be removed from
`zmq--live-sockets'. This is meant to cleanup any open sockets
before terminating `zmq-current-context' when Emacs is killed.
See `zmq-cleanup-on-exit'."
  (push sock zmq--live-sockets)
  sock)

(advice-add #'zmq-socket :filter-return #'zmq--live-sockets)

;; See `zmq-socket' type.
(zmq--ffi-wrapper "socket" :pointer ((context :context) (type :int)))
(zmq--ffi-wrapper "socket_monitor" :int ((sock :socket) (endpoint :string) (events :int)))

(defalias 'zmq-socket-monitor #'zmq--socket-monitor
  "Monitor for SOCK EVENTs on ENDPOINT.")

(zmq--ffi-wrapper "send_const" :int ((sock :socket) (buf :pointer) (len :size_t) (flags :int)))
(zmq--ffi-wrapper "send" :int ((sock :socket) (message :string) (len :size_t) (flags :int)))
;; NOTE: `zmq-recv' actually uses `zmq-recv-message' to let zmq handle any
;; buffers needed to receive a message.
(zmq--ffi-wrapper "recv" :int ((sock :socket) (buf :pointer) (len :size_t) (flags :int)))

(defun zmq-send-const (sock buf len &optional flags)
  "Send a message of constant bytes on SOCK.
BUF should be a constant memory byte buffer as obtained by
`with-ffi-tempory' or any other function which returns a
`user-ptr' to constant memory. LEN is the length of BUF and FLAGS
has the same meaning as `zmq-send'."
  (when (cl-assert (user-ptrp buf))
    (zmq--send-const sock buf len (or flags 0))))

(defun zmq-send (sock message &optional flags)
  "Send a single message on SOCK.
MESSAGE can either be a `zmq-message' or a string containing only
unibyte characters. FLAGS is a bitmask of flag options. See the
documentation of zmq_send in the C API for the values FLAGS can
take."
  (if (zmq-message-p message)
      (zmq-send-message message sock flags)
    (zmq--send sock message (length message) (or flags 0))))

(defun zmq-recv (sock &optional flags)
  "Receive a single message from SOCK with FLAGS.
FLAGS is a bitmask of flag options. See the documentation of
zmq_recv in the C API for the values FLAGS can take."
  (let ((message (zmq-message)))
    (unwind-protect
        (progn
          (zmq-recv-message message sock flags)
          (zmq-message-data message))
      (zmq-close-message message))))

(zmq--ffi-wrapper "bind" :int ((sock :socket) (endpoint :string)))
(zmq--ffi-wrapper "unbind" :int ((sock :socket) (endpoint :string)))
(zmq--ffi-wrapper "connect" :int ((sock :socket) (endpoint :string)))
(zmq--ffi-wrapper "disconnect" :int ((sock :socket) (endpoint :string)))
(zmq--ffi-wrapper "close" :int ((sock :socket)))

(defalias 'zmq-bind 'zmq--bind
  "Bind SOCK to ENDPOINT.")

(defalias 'zmq-unbind 'zmq--unbind
  "UnBIND SOCK from ENDPOINT.")

(defalias 'zmq-connect 'zmq--connect
  "Connect SOCK to ENDPOINT.")

(defalias 'zmq-disconnect 'zmq--disconnect
  "Disconnect SOCK from ENDPOINT.")

(defun zmq-close (sock)
  "Close SOCK."
  (setq zmq--live-sockets (delq sock zmq--live-sockets))
  (zmq--close sock))

(zmq--ffi-wrapper "setsockopt" :int ((sock :socket) (option :int) (value :pointer) (len :size_t)))
(zmq--ffi-wrapper "getsockopt" :int ((sock :socket) (option :int) (value :pointer) (len :pointer)))

(defun zmq-socket-set (sock option value)
  "Set SOCK OPTION to VALUE."
  (let ((buf-size 256)
        size)
    (with-ffi-temporary (buf buf-size)
      (cond
       ;; INT
       ((member option (list zmq-BACKLOG zmq-RATE
                             zmq-RECOVERY-IVL zmq-SNDHWM
                             zmq-SNDBUF zmq-SNDTIMEO zmq-RCVHWM
                             zmq-RCVBUF zmq-RCVTIMEO zmq-LINGER
                             zmq-RECONNECT-IVL
                             zmq-RECONNECT-IVL-MAX
                             zmq-MULTICAST-HOPS
                             zmq-MULTICAST-MAXTPDU
                             zmq-CONNECT-TIMEOUT
                             zmq-HANDSHAKE-IVL zmq-HEARTBEAT-IVL
                             zmq-HEARTBEAT-TIMEOUT
                             zmq-HEARTBEAT-TTL zmq-USE-FD
                             zmq-TCP-KEEPALIVE
                             zmq-TCP-KEEPALIVE-CNT
                             zmq-TCP-KEEPALIVE-IDLE
                             zmq-TCP-KEEPALIVE-INTVL
                             zmq-TCP-MAXRT zmq-TOS
                             zmq-VMCI-CONNECT-TIMEOUT))
        (setq size (ffi--type-size :int))
        (ffi--mem-set buf :int value))
       ;; UINT64
       ((member option (list zmq-AFFINITY zmq-VMCI-BUFFER-SIZE
                             zmq-VMCI-BUFFER-MAX-SIZE
                             zmq-VMCI-BUFFER-MIN-SIZE))
        (setq size (ffi--type-size :uint64))
        (ffi--mem-set buf :uint64 value))
       ;; INT64
       ((= option zmq-MAXMSGSIZE)
        (setq size (ffi--type-size :int64))
        (ffi--mem-set buf :int64 value))
       ;; INT with BOOL values
       ((member option (list zmq-CONFLATE zmq-CURVE-SERVER
                             zmq-GSSAPI-PLAINTEXT
                             zmq-GSSAPI-SERVER zmq-IMMEDIATE
                             zmq-INVERT-MATCHING zmq-IPV6
                             zmq-PLAIN-SERVER zmq-PROBE-ROUTER
                             zmq-REQ-CORRELATE zmq-REQ-RELAXED
                             zmq-ROUTER-HANDOVER
                             zmq-ROUTER-MANDATORY zmq-ROUTER-RAW
                             zmq-STREAM-NOTIFY zmq-XPUB-VERBOSE
                             zmq-XPUB-VERBOSER zmq-XPUB-MANUAL
                             zmq-XPUB-NODROP))
        (unless (booleanp value)
          (signal 'wrong-type-argument (list 'booleanp value)))

        (setq size (ffi--type-size :int))
        (ffi--mem-set buf :int64 (if value 1 0)))
       ;; STRING
       ((member option (list zmq-GSSAPI-PRINCIPAL
                             zmq-GSSAPI-SERVICE-PRINCIPAL
                             zmq-PLAIN-PASSWORD
                             zmq-PLAIN-USERNAME zmq-SOCKS-PROXY
                             zmq-ZAP-DOMAIN))
        (when (multibyte-string-p value)
          (signal 'wrong-type-argument (list '(not multibyte-string-p) value)))

        (setq size (length value))
        (unless (<= size buf-size)
          (error "Length of value too long"))
        (zmq--set-bytes buf value))
       ;; BINARY
       ((member option (list zmq-CONNECT-ROUTING-ID
                             zmq-ROUTING-ID zmq-SUBSCRIBE
                             zmq-UNSUBSCRIBE
                             zmq-XPUB-WELCOME-MSG))
        (when (multibyte-string-p value)
          (signal 'wrong-type-argument (list '(not multibyte-string-p) value)))

        (setq size (length value))
        (unless (<= size buf-size)
          (error "Length of value too long"))
        (zmq--set-bytes buf value))
       ;; CURVE
       ((member option (list zmq-CURVE-PUBLICKEY
                             zmq-CURVE-SECRETKEY
                             zmq-CURVE-SERVERKEY))
        (cond
         ((= (length value) 32)
          (setq size 32)
          (zmq--set-bytes buf value))
         ((= (length value) 40)
          (setq size 41)
          (zmq--set-bytes buf value)
          ;; `zmq--set-bytes' doesn't set the NULL byte
          (ffi--mem-set (ffi-pointer+ buf 40) :char 0))
         (t (signal 'args-out-of-range (list 'zmq-CURVE value)))))
       (t (error "Socket option not handled yet")))
      (zmq--setsockopt sock option buf size))))

(defun zmq-socket-get (sock option)
  "Get SOCK OPTION."
  (let ((buf-size 256))
    (with-ffi-temporaries ((len :size_t)
                           (buf buf-size))
      (cond
       ;; INT
       ((member option (list zmq-MECHANISM
                             zmq-BACKLOG zmq-RATE
                             zmq-RECOVERY-IVL zmq-SNDHWM
                             zmq-SNDBUF zmq-SNDTIMEO zmq-RCVHWM
                             zmq-RCVBUF zmq-RCVTIMEO zmq-LINGER
                             zmq-RECONNECT-IVL
                             zmq-RECONNECT-IVL-MAX
                             zmq-MULTICAST-HOPS
                             zmq-MULTICAST-MAXTPDU
                             zmq-CONNECT-TIMEOUT
                             zmq-HANDSHAKE-IVL zmq-HEARTBEAT-IVL
                             zmq-HEARTBEAT-TIMEOUT
                             zmq-HEARTBEAT-TTL zmq-USE-FD
                             zmq-EVENTS
                             zmq-TCP-KEEPALIVE
                             zmq-TCP-KEEPALIVE-CNT
                             zmq-TCP-KEEPALIVE-IDLE
                             zmq-TCP-KEEPALIVE-INTVL
                             zmq-TCP-MAXRT zmq-TOS
                             zmq-VMCI-CONNECT-TIMEOUT))
        (ffi--mem-set len :size_t (ffi--type-size :int))
        (zmq--getsockopt sock option buf len)
        (ffi--mem-ref buf :int))
       ;; UINT64
       ((member option (list zmq-AFFINITY zmq-VMCI-BUFFER-SIZE
                             zmq-VMCI-BUFFER-MAX-SIZE
                             zmq-VMCI-BUFFER-MIN-SIZE))
        (ffi--mem-set len :size_t (ffi--type-size :uint64))
        (zmq--getsockopt sock option buf len)
        (ffi--mem-ref buf :uint64))
       ;; INT64
       ((= option zmq-MAXMSGSIZE)
        (ffi--mem-set len :size_t (ffi--type-size :int64))
        (zmq--getsockopt sock option buf len)
        (ffi--mem-ref buf :int64))
       ;; TODO: Different on windows
       ((= option zmq-FD)
        (ffi--mem-set len :size_t (ffi--type-size :int))
        (zmq--getsockopt sock option buf len)
        (ffi--mem-ref buf :int))
       ;; INT with BOOL values
       ((member option (list zmq-IMMEDIATE zmq-INVERT-MATCHING
                             zmq-CONFLATE zmq-IPV6
                             zmq-PLAIN-SERVER
                             zmq-GSSAPI-PLAINTEXT
                             zmq-GSSAPI-SERVER zmq-RCVMORE))
        (ffi--mem-set len :size_t (ffi--type-size :int))
        (zmq--getsockopt sock option buf len)
        (= (ffi--mem-ref buf :int) 1))
       ;; BOOL
       ((= option zmq-THREAD-SAFE)
        (ffi--mem-set len :size_t (ffi--type-size :bool))
        (zmq--getsockopt sock option buf len)
        (ffi--mem-ref buf :bool))
       ;; STRINGS
       ((member option (list zmq-GSSAPI-PRINCIPAL zmq-GSSAPI-SERVICE-PRINCIPAL
                             zmq-LAST-ENDPOINT zmq-PLAIN-PASSWORD
                             zmq-PLAIN-USERNAME zmq-SOCKS-PROXY
                             zmq-ZAP-DOMAIN))
        (ffi--mem-set len :size_t buf-size)
        (zmq--getsockopt sock option buf len)
        (ffi-get-c-string buf))
       ;; BINARY
       ((member option (list zmq-ROUTING-ID))
        (ffi--mem-set len :size_t buf-size)
        (zmq--getsockopt sock option buf len)
        (zmq--get-bytes buf (ffi--mem-ref len :size_t)))
       ;; CURVE
       ((member option (list zmq-CURVE-PUBLICKEY
                             zmq-CURVE-SECRETKEY
                             zmq-CURVE-SERVERKEY))
        ;; Note that this always returns the string representation
        (ffi--mem-set len :size_t 41)
        (zmq--getsockopt sock option buf len)
        (ffi-get-c-string buf))
       (t (error "Socket option not handled yet"))))))

(provide 'zmq-ffi)

;; Local Variables:
;; byte-compile-warnings: (not unresolved)
;; End:

;;; zmq-ffi.el ends here
