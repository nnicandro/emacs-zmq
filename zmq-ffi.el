(eval-and-compile
  (require 'cl-lib)
  ;; Have these available at compile time to generate error codes during
  ;; compilation.
  (require 'zmq-constants)
  (require 'ffi)
  (define-ffi-library libzmq "libzmq"))

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

;; TODO: Use something like
;;
;;     cpp -dM -E zmq.h 2> /dev/null
;;
;; To extract the constants and errors from the zmq.h header file

(eval-and-compile
  ;; FIXME: Currently a hack to get the available errors.
  (defconst zmq--gen-error-codes-command
    "python -c \"
import errno
errs = ['(%d . zmq-%s)' % (i, errno.errorcode[i])
        for i in errno.errorcode.keys()]
print('\\'(' + '\\n'.join(errs) + ')')\""
    "A command that should print a list of cons cells with the
`car' being the error number and the `cdr' being the symbol
used to define the error corresponding to the error number.")

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
replaced with '-'."
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

;; TODO: Create proper elisp type errors for example:
;;
;;     zmq-ETERM -> zmq-context-terminated
(defmacro zmq--define-errors ()
  "Generate error symbols.

This macro defines an alist `zmq-error-alist' which has elements
with the form:

    (errno . errsym)

The alist is used in `zmq-error-handler' to associate the error
numbers returned by `zmq-errno' to their elisp equivalent error
symbol. This is so that we can properly signal the correct error
in emacs when an error occurs in ZMQ."
  ;; Defined in libzmq/include/zmq.h
  (let* ((HAUSNUMERO 156384712)
         (native-errors (list (cons (+ HAUSNUMERO 51) 'zmq-EFSM)
                              (cons (+ HAUSNUMERO 52) 'zmq-ENOCOMPATPROTO)
                              (cons (+ HAUSNUMERO 53) 'zmq-ETERM)
                              (cons (+ HAUSNUMERO 54) 'zmq-EMTHREAD)))
         ;; Hack to get the error codes on the system.
         (c-errors
          (eval (read (shell-command-to-string
                       zmq--gen-error-codes-command))))
         (errors (append c-errors native-errors)))
    `(progn
       (defconst zmq-error-alist (quote ,errors))
       (define-error 'zmq-ERROR "An error occured in ZMQ" 'error)
       ,@(cl-loop
          for (errno . errsym) in errors
          collect `(define-error ',errsym
                     ,(ffi-get-c-string (zmq-strerror errno)) 'zmq-ERROR)))))

;;; Memory handling functions

(defun zmq--get-bytes (buf size)
  "Get SIZE bytes of BUF and return a `unibyte-string'."
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
(eval-and-compile
  (define-ffi-function zmq-strerror "zmq_strerror" :pointer [:int] libzmq)
  (zmq--define-errors))

(defun zmq-error-handler (&rest data)
  "Called in the wrapped ZMQ functions when an error occurs.
This function raises the proper error depending on `zmq-errno' by
using `zmq-error-alist'."
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
                                (error "Context not created."))))))
  (-ptr nil :read-only t))

;; See the `zmq-context' type
(zmq--ffi-wrapper "ctx_new" :pointer () noerror)
(zmq--ffi-wrapper "ctx_set" :int ((context :context) (option :int) (value :int)))
(zmq--ffi-wrapper "ctx_get" :int ((context :context) (option :int)))
(zmq--ffi-wrapper "ctx_term" :int ((context :context)))
(zmq--ffi-wrapper "ctx_shutdown" :int ((context :context)))

(defun zmq-context-set (context option value)
  "Set a CONTEXT OPTION."
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
             (progn
               (zmq--ctx-term context)
               nil)
           (zmq-EINTR t)
           (error (signal (car err) (cdr err))))))

(defalias 'zmq-shutdown-context #'zmq--ctx-shutdown
  "Shutdown CONTEXT.")

;;; Encryption

(zmq--ffi-wrapper "z85_decode" :pointer ((decoded :pointer) (str :pointer)) noerror)
(zmq--ffi-wrapper "z85_encode" :pointer ((encoded :pointer) (str :pointer) (len :size_t)) noerror)
(zmq--ffi-wrapper "curve_keypair" :int ((public :pointer) (secret :pointer)))
(zmq--ffi-wrapper "curve_public" :int ((public :pointer) (secret :pointer)))

(defun zmq-z85-decode (key)
  "Decode a z85 encoded KEY."
  (unless (= (mod (length key) 5) 0)
    (signal 'args-out-of-range '("Length not a multiple of 5.")))
  (with-ffi-string (k key)
    (with-ffi-temporary (decoded (ceiling (* 0.8 (length key))))
      (when (ffi-pointer= decoded (zmq--z85-decode decoded k))
        (ffi-get-c-string decoded)))))

(defun zmq-z85-encode (data)
  "Encode DATA using the z85 encoding."
  (with-ffi-string (d data)
    (with-ffi-temporary (encoded (+ (ceiling (* 1.25 (length data))) 1))
      (when (ffi-pointer= encoded (zmq--z85-encode encoded d (length data)))
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
  (with-ffi-string (secret-key secret-key)
    (with-ffi-temporary (public-key 41)
      (zmq--curve-public public-key secret-key)
      (ffi-get-c-string public-key))))

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
        (progn
          (zmq--msg-copy dest message)
          dest)
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
  "Alist with the available metadata properties that can be
retrieved with `zmq-message-propery'.")

(zmq--ffi-wrapper "msg_set" :int ((message :message) (property :int) (value :int)))
(zmq--ffi-wrapper "msg_get" :int ((message :message) (property :int)))
(zmq--ffi-wrapper "msg_gets" :pointer ((message :message) (property :string)))
(zmq--ffi-wrapper "msg_routing_id" :uint32 ((message :message)))
(zmq--ffi-wrapper "msg_set_routing_id" :int ((message :message) (id :int)))

(defun zmq-message-set (message property value)
  "Set a PROPERTY of MESSAGE to VALUE."
  (when (and (booleanp value)
             (= property zmq-MORE))
    (setq value (if value 1 0)))
  (zmq--msg-set message property value))

(defun zmq-message-get (message property)
  "Get a PROPERTY of MESSAGE."
  (let ((value (zmq--msg-get message property)))
    (if (= property zmq-MORE)
        (= value 1)
      value)))

(defun zmq-message-property (message property)
  "Get a metadata PROPERTY of MESSAGE.

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
  (cl-loop
   for e in `(,zmq-POLLIN ,zmq-POLLOUT ,zmq-POLLERR)
   if (/= (logand e events) 0) collect e))

(zmq--ffi-wrapper "poll" :int ((items :pointer) (len :int) (timeout :long)))

(defun zmq-poll (items timeout)
  "Poll the list of `zmq-pollitem's in ITEMS until TIMEOUT."
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
                                  (error "Poller not created."))))))
    (-ptr nil :read-only t)
    (-socks-fds nil))

  ;; TODO: Handle windows machines
  ;; See `zmq-poller' type
  (zmq--ffi-wrapper "poller_new" :pointer ())
  (zmq--ffi-wrapper "poller_destroy" :int ((pollerp :pointer)))

  (defun zmq-poller-destroy (poller)
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
              (zmq-EINVAL t)
              (error (signal (car err) (cdr err))))
        (setq user-data (or user-data (ffi-null-pointer)))
        (if (integerp sock-or-fd)
            (zmq--poller-add-fd poller sock-or-fd user-data events)
          (zmq--poller-add poller sock-or-fd user-data events))
        (setf (zmq-poller--socks-fds poller)
              (cons sock-or-fd (zmq-poller--socks-fds poller))))))

  (defun zmq-poller-modify (poller sock-or-fd events)
    "Modify the EVENTS of SOCK-OR-FD that POLLER listens for."
    (let ((events (if (listp events) (apply #'logior events)
                    events)))
      (if (integerp sock-or-fd)
          (zmq--poller-modify-fd poller sock-or-fd events)
        (zmq--poller-modify poller sock-or-fd events))))

  (defun zmq-poller-remove (poller sock-or-fd)
    "Remove SOCK-OR-FD from POLLER."
    (when (condition-case err
              (progn
                (if (integerp sock-or-fd)
                    (zmq--poller-remove-fd poller sock-or-fd)
                  (zmq--poller-remove poller sock-or-fd))
                t)
            (zmq-EINVAL nil)
            (error (signal (car err) (cdr err))))
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
wait forever."
    (with-ffi-temporaries ((e zmq--poller-event-t))
      (condition-case err
          (when (>= (zmq--poller-wait poller e timeout) 0)
            (cons (zmq--poller-event-trigger poller e)
                  (zmq--split-poll-events (zmq--poller-event-t-events e))))
        (zmq-ETIMEDOUT nil)
        (error (signal (car err) (cdr err))))))

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
          (zmq-ETIMEDOUT nil)
          (error (signal (car err) (cdr err))))))))

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
  "Send LEN bytes from a constant memory BUF on SOCK with FLAGS."
  (when (cl-assert (user-ptrp buf))
    (zmq--send-const sock buf len (or flags 0))))

(defun zmq-send (sock message &optional flags)
  "Send a single message on SOCK."
  (if (zmq-message-p message)
      (zmq-send-message message sock flags)
    (zmq--send sock message (length message) (or flags 0))))

(defun zmq-recv (sock &optional flags)
  "Receive a single message from SOCK with FLAGS."
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
  "DisCONNECT SOCK from ENDPOINT.")

(defalias 'zmq-close 'zmq--close
  "Close SOCK.")

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
                             zmq-RECOVERY_IVL zmq-SNDHWM
                             zmq-SNDBUF zmq-SNDTIMEO zmq-RCVHWM
                             zmq-RCVBUF zmq-RCVTIMEO zmq-LINGER
                             zmq-RECONNECT_IVL
                             zmq-RECONNECT_IVL_MAX
                             zmq-MULTICAST_HOPS
                             zmq-MULTICAST_MAXTPDU
                             zmq-CONNECT_TIMEOUT
                             zmq-HANDSHAKE_IVL zmq-HEARTBEAT_IVL
                             zmq-HEARTBEAT_TIMEOUT
                             zmq-HEARTBEAT_TTL zmq-USE_FD
                             zmq-TCP_KEEPALIVE
                             zmq-TCP_KEEPALIVE_CNT
                             zmq-TCP_KEEPALIVE_IDLE
                             zmq-TCP_KEEPALIVE_INTVL
                             zmq-TCP_MAXRT zmq-TOS
                             zmq-VMCI_CONNECT_TIMEOUT))
        (setq size (ffi--type-size :int))
        (ffi--mem-set buf :int value))
       ;; UINT64
       ((member option (list zmq-AFFINITY zmq-VMCI_BUFFER_SIZE
                             zmq-VMCI_BUFFER_MAX_SIZE
                             zmq-VMCI_BUFFER_MIN_SIZE))
        (setq size (ffi--type-size :uint64))
        (ffi--mem-set buf :uint64 value))
       ;; INT64
       ((= option zmq-MAXMSGSIZE)
        (setq size (ffi--type-size :int64))
        (ffi--mem-set buf :int64 value))
       ;; INT with BOOL values
       ((member option (list zmq-CONFLATE zmq-CURVE_SERVER
                             zmq-GSSAPI_PLAINTEXT
                             zmq-GSSAPI_SERVER zmq-IMMEDIATE
                             zmq-INVERT_MATCHING zmq-IPV6
                             zmq-PLAIN_SERVER zmq-PROBE_ROUTER
                             zmq-REQ_CORRELATE zmq-REQ_RELAXED
                             zmq-ROUTER_HANDOVER
                             zmq-ROUTER_MANDATORY zmq-ROUTER_RAW
                             zmq-STREAM_NOTIFY zmq-XPUB_VERBOSE
                             zmq-XPUB_VERBOSER zmq-XPUB_MANUAL
                             zmq-XPUB_NODROP))
        (unless (booleanp value)
          (signal 'wrong-type-argument (list 'booleanp value)))

        (setq size (ffi--type-size :int))
        (ffi--mem-set buf :int64 (if value 1 0)))
       ;; STRING
       ((member option (list zmq-GSSAPI_PRINCIPAL
                             zmq-GSSAPI_SERVICE_PRINCIPAL
                             zmq-PLAIN_PASSWORD
                             zmq-PLAIN_USERNAME zmq-SOCKS_PROXY
                             zmq-ZAP_DOMAIN))
        (when (multibyte-string-p value)
          (signal 'wrong-type-argument (list '(not multibyte-string-p) value)))

        (setq size (length value))
        (unless (<= size buf-size)
          (error "Length of value too long."))
        (zmq--set-bytes buf value))
       ;; BINARY
       ((member option (list zmq-CONNECT_ROUTING_ID
                             zmq-ROUTING_ID zmq-SUBSCRIBE
                             zmq-UNSUBSCRIBE
                             zmq-XPUB_WELCOME_MSG))
        (when (multibyte-string-p value)
          (signal 'wrong-type-argument (list '(not multibyte-string-p) value)))

        (setq size (length value))
        (unless (<= size buf-size)
          (error "Length of value too long."))
        (zmq--set-bytes buf value))
       ;; CURVE
       ((member option (list zmq-CURVE_PUBLICKEY
                             zmq-CURVE_SECRETKEY
                             zmq-CURVE_SERVERKEY))
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
       (t (error "Socket option not handled yet.")))
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
                             zmq-RECOVERY_IVL zmq-SNDHWM
                             zmq-SNDBUF zmq-SNDTIMEO zmq-RCVHWM
                             zmq-RCVBUF zmq-RCVTIMEO zmq-LINGER
                             zmq-RECONNECT_IVL
                             zmq-RECONNECT_IVL_MAX
                             zmq-MULTICAST_HOPS
                             zmq-MULTICAST_MAXTPDU
                             zmq-CONNECT_TIMEOUT
                             zmq-HANDSHAKE_IVL zmq-HEARTBEAT_IVL
                             zmq-HEARTBEAT_TIMEOUT
                             zmq-HEARTBEAT_TTL zmq-USE_FD
                             zmq-EVENTS
                             zmq-TCP_KEEPALIVE
                             zmq-TCP_KEEPALIVE_CNT
                             zmq-TCP_KEEPALIVE_IDLE
                             zmq-TCP_KEEPALIVE_INTVL
                             zmq-TCP_MAXRT zmq-TOS
                             zmq-VMCI_CONNECT_TIMEOUT))
        (ffi--mem-set len :size_t (ffi--type-size :int))
        (zmq--getsockopt sock option buf len)
        (ffi--mem-ref buf :int))
       ;; UINT64
       ((member option (list zmq-AFFINITY zmq-VMCI_BUFFER_SIZE
                             zmq-VMCI_BUFFER_MAX_SIZE
                             zmq-VMCI_BUFFER_MIN_SIZE))
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
       ((member option (list zmq-IMMEDIATE zmq-INVERT_MATCHING
                             zmq-CONFLATE zmq-IPV6
                             zmq-PLAIN_SERVER
                             zmq-GSSAPI_PLAINTEXT
                             zmq-GSSAPI_SERVER zmq-RCVMORE))
        (ffi--mem-set len :size_t (ffi--type-size :int))
        (zmq--getsockopt sock option buf len)
        (= (ffi--mem-ref buf :int) 1))
       ;; BOOL
       ((= option zmq-THREAD_SAFE)
        (ffi--mem-set len :size_t (ffi--type-size :bool))
        (zmq--getsockopt sock option buf len)
        (ffi--mem-ref buf :bool))
       ;; STRINGS
       ((member option (list zmq-GSSAPI_PRINCIPAL zmq-GSSAPI_SERVICE_PRINCIPAL
                             zmq-LAST_ENDPOINT zmq-PLAIN_PASSWORD
                             zmq-PLAIN_USERNAME zmq-SOCKS_PROXY
                             zmq-ZAP_DOMAIN))
        (ffi--mem-set len :size_t buf-size)
        (zmq--getsockopt sock option buf len)
        (ffi-get-c-string buf))
       ;; BINARY
       ((member option (list zmq-ROUTING_ID))
        (ffi--mem-set len :size_t buf-size)
        (zmq--getsockopt sock option buf len)
        (zmq--get-bytes buf (ffi--mem-ref len :size_t)))
       ;; CURVE
       ((member option (list zmq-CURVE_PUBLICKEY
                             zmq-CURVE_SECRETKEY
                             zmq-CURVE_SERVERKEY))
        ;; Note that this always returns the string representation
        (ffi--mem-set len :size_t 41)
        (zmq--getsockopt sock option buf len)
        (ffi-get-c-string buf))
       (t (error "Socket option not handled yet."))))))

(provide 'zmq-ffi)

;; Local Variables:
;; byte-compile-warnings: (not unresolved)
;; End:
