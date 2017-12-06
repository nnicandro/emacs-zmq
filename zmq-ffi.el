(require 'cl-lib)
(require 'zmq-constants)
(eval-and-compile
  ;; Have these available at compile time to generate error codes during
  ;; compilation.
  (require 'ffi)
  (define-ffi-library libzmq "libzmq"))

;;; Types

;; TODO: The fd field is different on windows
(define-ffi-struct zmq-pollitem
  (socket :type :pointer)
  (fd :type :int)
  (events :type :short)
  (revents :type :short))
(define-ffi-array zmq-msg :char 64)
(define-ffi-array zmq-work-buffer :char 256)
(cl-defstruct (zmq-poller
               (:constructor
                zmq-poller
                (&aux (-ptr (or (zmq--poller-new)
                                (error "Poller not created."))))))
  (-ptr nil :read-only t))

(cl-defstruct (zmq-poller-event
               (:constructor
                zmq-poller-event
                (socket events &optional fd user-data)))
  (socket nil :read-only t)
  (fd -1 :read-only t)
  (user-data (ffi-null-pointer) :read-only t)
  (events 0))

(cl-defstruct (zmq-context
               (:constructor
                zmq-context
                (&aux (-ptr (or (zmq--ctx-new)
                                (error "Context not created."))))))
  (-ptr nil :read-only t))

(cl-defstruct
    (zmq-socket
     (:constructor nil)
     (:constructor
      zmq-socket
      (ctx type &aux (-ptr
                      (unless (integerp type)
                        (signal 'wrong-type-argument '(integerp)))
                      (zmq--socket ctx type)))))
  (-ptr nil :read-only t))

(cl-defstruct
    (zmq-message
     (:constructor
      zmq-message
      (&optional
       size-or-data &aux
       (-ptr (let ((val size-or-data)
                   (msg (ffi-allocate zmq-msg)))
               (cond
                ((stringp val)
                 (zmq--msg-init-data msg val))
                ((integerp val)
                 (zmq--msg-init-size msg val))
                ((null val)
                 (zmq--msg-init msg))
                ;; Special case, don't initialize at all
                ((eq val t))
                (t (ffi-free msg)
                   (signal 'wrong-type-argument
                           (list (format "Can't initialize message with %s"
                                         val)))))
               msg)))))
  (-ptr nil :read-only t))

;;; FFI wrapper

(eval-and-compile
  (defun zmq--ffi-normalize-arg-types (args arg-types)
    "Handle ARG-TYPES specific to ZMQ.

THis function converts any ARG-TYPES with the special values
`:socket', `:context', `:poller', `:message', or `:string' to
`:pointer' and returns a form (STRING-BINDINGS . ARG-CHECKERS)
where STRING-BINDINGS is a list that is used to convert any
`:string' ARGS into their equivalent string representations
required by ZMQ, i.e. c-strings.

If ARGS has one of the other special types besides `:string' then
a form

    (if (zmq-<special type>-p ARG) (setq ARG (zmq-<special type>--ptr))
      (signal 'wrong-type-argument '(zmq-<special type>-p)))

will be generated and ARG-CHECKERS will contain a list of all
these argument checking forms. Note that when an argument is of
the valid type, it is unwrapped into its c-pointer equivalent to
be passed to the ffi function."
    ;; Mapping types to cl-defstruct types
    (let ((type-struct '((:socket . "zmq-socket")
                         (:context . "zmq-context")
                         (:poller . "zmq-poller")
                         (:message . "zmq-message"))))
      ;; Handle special `:string' arguments which gets converted into
      ;; `:pointer' and wrapped using `with-ffi-strings' when calling
      ;; the function.
      ;;
      ;; So if the arg-types is ((arg1 :string) (arg2 :pointer))
      ;; the resulting form is
      ;;
      ;; (with-ffi-strings ((arg1 (encode-coding-string arg1 'utf-8)))
      ;;   <body>)
      ;;
      ;; Which copies arg1 to a c-string pointer and and is available
      ;; as arg1 in <body>.
      (cl-loop
       with st = nil
       for i from 0 to (1- (length arg-types))
       when (eq (aref arg-types i) :string)
       do (aset arg-types i :pointer)
       and collect (let ((arg (nth i args)))
                     (list arg arg))
       into string-bindings
       when (setq st (assoc (aref arg-types i) type-struct))
       do (aset arg-types i :pointer)
       and collect
       (let ((pred (make-symbol (concat (cdr st) "-p")))
             (arg (nth i args))
             (unwrap (make-symbol (concat (cdr st) "--ptr"))))
         `(if (,pred ,arg)
              (setq ,arg (,unwrap ,arg))
            (signal 'wrong-type-argument (list ',pred))))
       into arg-checkers
       finally return (cons string-bindings arg-checkers)))))

(defmacro zmq--ffi-wrapper (c-name return-type arg-types &optional noerror)
  (declare (debug t) (indent 1))
  (let* ((fname (subst-char-in-string ?_ ?- c-name))
         (c-name (concat "zmq_" c-name))
         (ffi-name (make-symbol (concat "zmq--" fname "-1"))))
    (macroexpand-all
     (let* ((wrapped-name (make-symbol (concat "zmq--" fname)))
            (args (if (listp arg-types)
                      (cl-loop
                       for (name type) in arg-types
                       collect name and collect type into types
                       finally do (setq arg-types (apply #'vector types)))
                    (cl-loop repeat (length arg-types)
                             collect (cl-gensym))))
            (body `(let ((ret (,ffi-name ,@args)))
                     ;; Depending on the return-type, check if an error is
                     ;; set.
                     (if ,(cl-case return-type
                            (:pointer '(ffi-pointer-null-p ret))
                            (:int '(= ret -1))
                            ;; Just return without checking for errors
                            (t nil))
                         ,(if noerror 'nil '(zmq-error-handler))
                       ret)))
            bindings-checkers
            string-bindings
            arg-checkers)
       (setq bindings-checkers (zmq--normalize-arg-types args arg-types)
             string-bindings (car bindings-checkers)
             arg-checkers (cdr bindings-checkers))
       `(progn
          (define-ffi-function
            ,ffi-name ,c-name ,return-type ,arg-types libzmq)
          (defun ,wrapped-name ,args
            ,@(when arg-checkers
                arg-checkers)
            ,(if string-bindings
                 `(with-ffi-strings ,string-bindings
                    ,body)
               body)))))))

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
                              (cons  (+ HAUSNUMERO 52) 'zmq-ENOCOMPATPROTO)
                              (cons  (+ HAUSNUMERO 53) 'zmq-ETERM)
                              (cons  (+ HAUSNUMERO 54) 'zmq-EMTHREAD)))
         ;; Hack to get the error codes on the system.
         (c-errors
          (eval (read
                 (shell-command-to-string
                  (concat
                   "python -c "
                   "\"import errno; "
                   "print('\\'(' + '\\n'.join(['(%d . zmq-%s)' "
                   "% (i, errno.errorcode[i]) "
                   "for i in errno.errorcode.keys()]) + ')')\"")))))
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

(defun zmq--set-buf (buf type-or-value &optional value)
  "Set `buf' depending on TYPE-OR-VALUE and VALUE.

If TYPE-OR-VALUE is non-nil, then it can only be one of the
primitive types such as `:int' or `:char' and VALUE must be
non-nil and should have the type corresponding to TYPE-OR-VALUE.

If VALUE is nil, then TYPE-OR-VALUE must be a string which is
passed to `zmq--set-bytes'."
  (cond
   ((and (null value) (stringp type-or-value))
    (setq value type-or-value)
    (zmq--set-bytes buf value))
   ((and value (keywordp type-or-value))
    (ffi--mem-set buf type-or-value value))
   (t (signal 'wrong-type-argument nil))))

;;; Utility functions

(eval-and-compile
  (zmq--ffi-wrapper "has" :int [:string] noerror)
  (zmq--ffi-wrapper "version" :void [:pointer :pointer :pointer] noerror)

  (defun zmq-has (capability)
    "Does ZMQ have CAPABILITY?"
    (= (zmq--has capability)))

  (defun zmq-version ()
    "Get the version of ZMQ."
    (with-ffi-temporaries ((major :int)
                           (minor :int)
                           (patch :int))
      (zmq--version major minor patch)
      (format "%d.%d.%d" major minor patch))))

;;; Error handling

;; These are used in `zmq--ffi-wrapper' so don't try to wrap them.
(define-ffi-function zmq-errno "zmq_errno" :int [] libzmq)
;; Used in the `zmq--define-errors' macro.
(eval-and-compile
  (define-ffi-function zmq-strerror "zmq_strerror" :pointer [:int] libzmq))

(zmq--define-errors)

(defun zmq-error-handler (&rest data)
  "Called in the wrapped ZMQ functions when an error occurs.
This function raises the proper error depedning on `zmq-errno'"
  (let* ((errno (zmq-errno))
         (errsym (or (cdr (assoc errno zmq-error-alist))
                     'zmq-ERROR)))
    (signal errsym (list (get errsym 'error-message)))))

;;; Contexts

;; See the `zmq-context' type
(zmq--ffi-wrapper "ctx_new" :pointer [] noerror)

(zmq--ffi-wrapper "ctx_set" :int ((context :context) (option :int) (value :int)))
(defun zmq-context-set (context option value)
  "Set a CONTEXT OPTION."
  (zmq--ctx-set context option))

(zmq--ffi-wrapper "ctx_get" :int ((context :context) (option :int)))
(defun zmq-context-get (context option)
  "Get a CONTEXT OPTION."
  (zmq--ctx-get context option))

(zmq--ffi-wrapper "ctx_term" :int ((context :context)))
(defun zmq-terminate-context (context)
  "Terminate CONTEXT."
  (zmq--ctx-term context))

(zmq--ffi-wrapper "ctx_shutdown" :int ((context :context)))
(defun zmq-shutdown-context (context)
  "Shutdown CONTEXT."
  (zmq--ctx-shutdown context))

;;; Encryption

(zmq--ffi-wrapper "z85_decode" :pointer [:pointer :pointer] noerror)
(zmq--ffi-wrapper "z85_encode" :pointer [:pointer :pointer :size_t] noerror)
(zmq--ffi-wrapper "curve_keypair" :int [:pointer :pointer])
(zmq--ffi-wrapper "curve_public" :int [:pointer :pointer])

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

(zmq--ffi-wrapper "msg_close" :int [:message])
(defun zmq-close-message (message)
  "Close a MESSAGE."
  (unless (zmq-message-p message)
    (signal 'wrong-type-argument 'zmq-message-p))
  (unwind-protect
      (zmq--msg-close message)
    (ffi-free (zmq-message--ptr message))))

(zmq--ffi-wrapper "msg_copy" :int [:message :message])
(defun zmq-copy-message (message)
  "Copy a MESSAGE."
  (let ((dest (zmq-message)))
    (unwind-protect
        (progn
          (zmq--msg-copy dest message)
          dest)
      (zmq-close-message dest))))

(zmq--ffi-wrapper "msg_data" :pointer [:message] noerror)
(defun zmq-message-data (message)
  "Get the data of MESSAGE."
  (let ((data (zmq--msg-data message)))
    (when data
      (zmq--get-bytes data (zmq--msg-size message)))))

(zmq--ffi-wrapper "msg_get" :int ((message :message) (property :int)))
(defun zmq-message-get (message property)
  "Get a PROPERTY of MESSAGE."
  (zmq--msg-get message property))

(zmq--ffi-wrapper "msg_gets" :pointer [:message :pointer])

(defconst zmq-message-properties '((:socket-type . "Socket-Type")
                                   (:identity . "Identity")
                                   (:resource . "Resource")
                                   (:peer-address . "Peer-Address"))
  "Alist with the available metadata properites that can be
retrieved with `zmq-message-propery'.")

(defun zmq-message-property (message property)
  "Get a metadata PROPERTY of MESSAGE.

PROPERTY is a keyword and can only be one of
`zmq-message-properties'."
  (let ((prop (cdr (assoc property zmq-message-properties))))
    (unless prop
      (signal 'args-out-of-range (list (mapcar #'car zmq-message-properties))))
    (with-ffi-string (prop (encode-coding-string prop 'utf-8))
      (ffi-get-c-string (zmq--msg-gets message prop)))))

(zmq--ffi-wrapper "msg_more" :int [:pointer])
(defun zmq-message-more-p (message)
  "Does MESSAGE have more parts?"
  (= (zmq--msg-more message) 1))

;; See `zmq-message' type
(zmq--ffi-wrapper "msg_init_data"
  :int ((message :message) (data :string) (len :size_t)
        (free-fn :pointer) (hint :pointer)))
(zmq--ffi-wrapper "msg_init_size" :int ((message :message) (size :size_t)))
(zmq--ffi-wrapper "msg_init" :int ((message :message)))

(zmq--ffi-wrapper "msg_move" :int ((dest :message) (src :message)))
(defun zmq-move-message (dest src)
  "Move a message from SRC to DEST."
  (zmq--msg-move dest src))

(zmq--ffi-wrapper "msg_recv" :int ((message :message) (sock :pointer) (flags :int)))
(defun zmq-recv-message (message sock &optional flags)
  "Receive a MESSAGE from SOCK with additional FLAGS.

MESSAGE should be an initialized message."
  (zmq--msg-recv message sock (or flags 0)))

(zmq--ffi-wrapper "msg_send" :int ((message :message) (sock :socket) (flags :int)))
(defun zmq-send-message (message sock flags)
  "Send a MESSAGE on SOCK with additional FLAGS."
  (zmq--msg-send message sock (or flags 0)))

(zmq--ffi-wrapper "msg_routing_id" :uint32 ((message :message)))
(defun zmq-message-id (message)
  "Get the routing ID of MESSAGE."
  (zmq--msg-routing-id message))

(zmq--ffi-wrapper "msg_set_routing_id" :int ((message :message) (id :int)))
(defun zmq-message-set-id (message id)
  "Set the routing ID of MESSAGE."
  (zmq--msg-set-routing-id message id))

(zmq--ffi-wrapper "msg_set" :int ((message :message) (property :int) (value :int)))
(defun zmq-message-set (message property value)
  "Set a PROPERTY of MESSAGE to VALUE."
  (zmq--msg-set message property value))

(zmq--ffi-wrapper "msg_size" :size_t ((message :message)))
(defun zmq-message-size (message)
  "Get the size of MESSAGE."
  (zmq--msg-size message))

(defun zmq--msg-init-data-free (data hint)
  (while (= (zmq-atomic-counter-value zmq--pending-delete-semaphore) 0)
    (sleep-for 0 1))
  (zmq-atomic-counter-dec zmq--pending-delete-semaphore)
  (ffi-free data)
  (zmq-atomic-counter-inc zmq--pending-delete-semaphore)
  (zmq-atomic-counter-dec zmq--pending-delete-counter))

(defvar zmq--msg-init-data-free-fn
  (ffi-make-closure (ffi--prep-cif :void [:pointer :pointer])
                    'zmq--msg-init-data-free)
  "Function pointer for use in `zmq-msg-init-data'.")

(defun zmq-msg-init-data (message data)
  ;; Closure's to lisp functions don't work exactly.
  (error "Not yet implemented.")
  (if (> (length data) 0)
      (let* ((len (length data))
             (buf (ffi-allocate len)))
        (zmq--set-bytes buf data)
        (zmq--msg-init-data
         message buf len
         zmq--msg-init-data-free-fn (ffi-null-pointer))
        (zmq--atomic-counter-inc zmq--pending-delete-counter)
        message)
    (zmq-msg-init message)))

;;; Polling

(zmq--ffi-wrapper "poll" :int [:pointer :int :long])

(defun zmq-pollitem-new (&rest args)
  (let ((item (ffi-allocate zmq-pollitem))
        (val nil))
    (setf (zmq-pollitem-sock item)
          (if (setq val (plist-get args :sock))
              val
            (ffi-null-pointer)))
    (setf (zmq-pollitem-fd item)
          (if (setq val (plist-get args :fd))
              val
            -1))
    (setf (zmq-pollitem-fd item)
          (if (setq val (plist-get args :events))
              val
            0))
    (setf (zmq-pollitem-fd item)
          (if (setq val (plist-get args :revents))
              val
            0))
    item))

(defun zmq-poll (items timeout)
  (let ((pointer-size (ffi--type-size :pointer)))
    (with-ffi-temporary (head (* (length items) pointer-size))
      (cl-loop
       with pointer = head
       for item in items do (ffi--mem-set pointer :pointer item)
       (setq pointer (ffi-pointer+ pointer pointer-size)))
      (zmq--poll head (length items) timeout))))

;;; Proxy

(zmq--ffi-wrapper "proxy_steerable"
  :int ((frontend :pointer) (backend :pointer)
        (capture :pointer) (control :pointer)))

(zmq--ffi-wrapper "proxy"
  :int ((frontend :pointer) (backend :pointer) (capture :pointer)))

;;; Sockets

(zmq--ffi-wrapper "send_const" :int [:socket :pointer :size_t :int])
(defun zmq-send-const (sock buf len &optional flags)
  "Send LEN bytes from a constant memory BUF on SOCK with FLAGS."
  (when (cl-assert (user-ptrp buf))
    (zmq--send-const sock buf len (or flags 0))))

(zmq--ffi-wrapper "send" :int [:socket :pointer :size_t :int])
(defun zmq-send (sock message &optional flags)
  "Send a message on SOCK."
  ;; NOTE: Try to avoid `zmq-msg-init-data' because closures don't work
  ;; properly with the ffi interface.
  (with-ffi-string (buf message)
    (zmq--send sock buf (length message) (or flags 0))))

(defun zmq-send-multipart (sock parts &optional flags)
  "Send a multipart message with PARTS on SOCK with FLAGS."
  (while (car parts)
    (zmq-send sock (car parts) (when (cdr parts) zmq-SNDMORE))))

(zmq--ffi-wrapper "recv" :int [:socket :pointer :size_t :int])
(defun zmq-recv (sock &optional flags)
  "Receive a message from SOCK with FLAGS."
  ;; NOTE: Uses `zmq--msg-recv' instead of `zmq--recv'
  (let ((message (zmq-message)))
    (unwind-protect
        (progn
          (zmq--msg-recv message sock (or flags 0))
          (zmq-message-data message))
      (zmq-close-message message))))

(defun zmq-recv-multipart (sock)
  "Receive a multipart message from SOCK."
  (let (res)
    (let ((part (zmq-message)))
      (unwind-protect
          (catch 'recvd
            (while t
              (zmq-recv-message part sock)
              (setq res (cons (zmq-message-data part) res))
              (unless (zmq-message-more-p part)
                (throw 'recvd (nreverse res)))
              (zmq-init-message part)))
        (zmq-close-message part)))))

;; See `zmq-socket' type.
(zmq--ffi-wrapper "socket" :pointer ((context :context) (type :int)))

(zmq--ffi-wrapper "socket_monitor" :int ((sock :socket) (endpoint :string) (events :int)))
(defun zmq-socket-monitor (sock endpoint events)
  "Monitor for SOCK EVENTs on ENDPOINT."
  (zmq--socket-monitor sock endpoint events))

(zmq--ffi-wrapper "bind" :int ((sock :socket) (endpoint :string)))
(defun zmq-bind (sock endpoint)
  "Bind SOCK to ENDPOINT."
  (zmq--bind sock endpoint))

(zmq--ffi-wrapper "unbind" :int ((sock :socket) (endpoint :string)))
(defun zmq-unbind (sock endpoint)
  "UnBIND SOCK from ENDPOINT."
  (zmq--unbind sock endpoint))

(zmq--ffi-wrapper "connect" :int ((sock :socket) (endpoint :string)))
(defun zmq-connect (sock endpoint)
  "Connect SOCK to ENDPOINT."
  (zmq--connect sock endpoint))

(zmq--ffi-wrapper "disconnect" :int ((sock :socket) (endpoint :string)))
(defun zmq-disconnect (sock endpoint)
  "DisCONNECT SOCK from ENDPOINT."
  (zmq--disconnect sock endpoint))

(zmq--ffi-wrapper "close" :int ((sock :socket)))
(defun zmq-close (sock)
  "Close SOCK."
  (zmq--close sock))

(zmq--ffi-wrapper "setsockopt" :int [:socket :int :pointer :size_t])
(defun zmq-setsockopt (sock option value)
  "Set SOCK OPTION to VALUE."
  (let (size)
    (with-ffi-temporary (buf zmq-work-buffer)
      (cond
       ;; INT
       ((member option (list zmq-BACKLOG zmq-CONNECT_TIMEOUT zmq-LINGER
                             zmq-RCVTIMEO zmq-HANDSHAKE_IVL zmq-HEARTBEAT_IVL
                             zmq-HEARTBEAT_TIMEOUT zmq-HEARTBEAT_TTL
                             zmq-MULTICAST_HOPS zmq-MULTICAST_MAXTPDU
                             zmq-USE_FD zmq-RATE zmq-RCVBUF zmq-RCVHWM
                             zmq-RCVTIMEO zmq-RECONNECT_IVL zmq-RECONNECT_IVL_MAX
                             zmq-RECOVERY_IVL zmq-TCP_KEEPALIVE
                             zmq-TCP_KEEPALIVE_CNT zmq-TCP_KEEPALIVE_IDLE
                             zmq-TCP_KEEPALIVE_INTVL zmq-TCP_MAXRT zmq-TOS
                             zmq-VMCI_CONNECT_TIMEOUT))
        (unless (integerp value)
          (signal 'wrong-type-argument (list 'integerp value)))

        (setq size (ffi--type-size :int))
        (zmq--set-buf buf :int value))
       ;; UINT64
       ((member option (list zmq-AFFINITY zmq-VMCI_BUFFER_SIZE
                             zmq-VMCI_BUFFER_MAX_SIZE zmq-VMCI_BUFFER_MIN_SIZE))
        (unless (integerp value)
          (signal 'wrong-type-argument (list 'integerp value)))

        (setq size (ffi--type-size :uint64))
        (zmq--set-buf buf :uint64 value))
       ;; INT64
       ((member option (list zmq-MAXMSGSIZE))
        (unless (integerp value)
          (signal 'wrong-type-argument (list 'integerp value)))

        (setq size (ffi--type-size :int64))
        (zmq--set-buf buf :int64 value))
       ;; INT with BOOL values
       ((member option (list zmq-CONFLATE zmq-CURVE_SERVER
                             zmq-GSSAPI_PLAINTEXT zmq-GSSAPI_SERVER
                             zmq-IMMEDIATE zmq-INVERT_MATCHING zmq-IPV6
                             zmq-PLAIN_SERVER zmq-PROBE_ROUTER zmq-REQ_CORRELATE
                             zmq-REQ_RELAXED zmq-ROUTER_HANDOVER
                             zmq-ROUTER_MANDATORY zmq-ROUTER_RAW zmq-SNDBUF
                             zmq-SNDHWM zmq-SNDTIMEO zmq-STREAM_NOTIFY
                             zmq-XPUB_VERBOSE zmq-XPUB_VERBOSER zmq-XPUB_MANUAL
                             zmq-XPUB_NODROP))
        (unless (booleanp value)
          (signal 'wrong-type-argument (list 'booleanp value)))

        (setq size (ffi--type-size :int))
        (zmq--set-buf buf :int64 (if value 1 0)))
       ;; STRING
       ((member option (list zmq-GSSAPI_PRINCIPAL zmq-GSSAPI_SERVICE_PRINCIPAL
                             zmq-PLAIN_PASSWORD zmq-PLAIN_USERNAME
                             zmq-SOCKS_PROXY zmq-ZAP_DOMAIN))
        (when (multibyte-string-p value)
          (signal 'wrong-type-argument (list '(not multibyte-string-p) value)))

        (setq size (1+ (length value)))
        (unless (<= size (ffi--type-size zmq-work-buffer))
          (error "Length of value too long."))
        (zmq--set-buf buf value))
       ;; BINARY
       ((member option (list zmq-CONNECT_ROUTING_ID zmq-ROUTING_ID zmq-SUBSCRIBE
                             zmq-UNSUBSCRIBE zmq-XPUB_WELCOME_MSG))
        (when (multibyte-string-p value)
          (signal 'wrong-type-argument (list '(not multibyte-string-p) value)))

        (setq size (length value))
        ;; account for extra null byte added in `zmq--set-bytes'
        (unless (<= size (ffi--type-size zmq-work-buffer))
          (error "Length of value too long."))
        (zmq--set-buf buf value))
       ;; CURVE
       ((member option (list zmq-CURVE_PUBLICKEY zmq-CURVE_SECRETKEY
                             zmq-CURVE_SERVERKEY))
        (cond
         ((= (length value) 32)
          ;; don't copy the NULL byte
          (setq size 32)
          (zmq--set-buf buf value))
         ((= (length value) 40)
          (setq size 41)
          (zmq--set-buf buf value))
         (t (signal 'args-out-of-range (list 'zmq-CURVE value)))))
       (t (error "Socket option not handled yet.")))
      (zmq--setsockopt sock option buf size))))

(zmq--ffi-wrapper "getsockopt" :int ((sock :socket) (option :int) (value :pointer) (len :pointer)))
(defun zmq-getsockopt (sock option)
  "Get SOCK OPTION."
  (with-ffi-temporaries ((len :size_t)
                         (buf zmq-work-buffer))
    (ffi--mem-set len :size_t (ffi--type-size zmq-work-buffer))
    (zmq--getsockopt sock option buf len)
    (cond
     ;; INT
     ((member option (list zmq-HANDSHAKE_IVL zmq-BACKLOG
                           zmq-CONNECT_TIMEOUT zmq-EVENTS
                           zmq-LINGER zmq-MECHANISM
                           zmq-MULTICAST_HOPS zmq-MULTICAST_MAXTPDU
                           zmq-PLAIN_SERVER zmq-USE_FD zmq-RATE zmq-RCVBUF
                           zmq-RCVHWM zmq-RCVTIMEO zmq-RECONNECT_IVL
                           zmq-RECONNECT_IVL_MAX zmq-RECOVERY_IVL
                           zmq-SNDBUF zmq-SNDHWM zmq-SNDTIMEO
                           zmq-TCP_KEEPALIVE zmq-TCP_KEEPALIVE_CNT
                           zmq-TCP_KEEPALIVE_IDLE zmq-TCP_KEEPALIVE_INTVL
                           zmq-TCP_MAXRT zmq-TOS zmq-TYPE
                           zmq-VMCI_CONNECT_TIMEOUT))
      (ffi--mem-ref buf :int))
     ;; UINT64
     ((member option (list zmq-AFFINITY zmq-VMCI_BUFFER_SIZE
                           zmq-VMCI_BUFFER_MIN_SIZE
                           zmq-VMCI_BUFFER_MAX_SIZE))
      (ffi--mem-ref buf :uint64))
     ;; INT64
     ((= option zmq-MAXMSGSIZE)
      (ffi--mem-ref buf :int64))
     ;; TODO: Different on windows
     ((= option zmq-FD)
      (ffi--mem-ref buf :int))
     ;; INT with BOOL values
     ((member option (list zmq-IMMEDIATE zmq-INVERT_MATCHING
                           zmq-IPV6 zmq-GSSAPI_PLAINTEXT
                           zmq-GSSAPI_SERVER
                           zmq-RCVMORE))
      (= (ffi--mem-ref buf :int) 1))
     ;; BOOL
     ((= option zmq-THREAD_SAFE)
      (ffi--mem-ref buf :bool))
     ;; STRINGS
     ((member option (list zmq-GSSAPI_PRINCIPAL zmq-GSSAPI_SERVICE_PRINCIPAL
                           zmq-LAST_ENDPOINT zmq-PLAIN_PASSWORD
                           zmq-PLAIN_USERNAME zmq-SOCKS_PROXY
                           zmq-ZAP_DOMAIN))
      (ffi-get-c-string buf))
     ;; BINARY
     ((= option zmq-ROUTING_ID)
      (zmq--get-bytes buf (ffi--mem-ref len :size_t)))
     ;; CURVE
     ((member option (list zmq-CURVE_PUBLICKEY zmq-CURVE_SECRETKEY
                           zmq-CURVE_SERVERKEY))
      (let ((len (ffi--mem-ref len :size_t)))
        (cond
         ;; TODO: What happens when the value is NULL (the default)? Does
         ;; zmq--getsockopt error out in that case?
         ((= len 0) nil)
         ((= len 32)
          (zmq--get-bytes buf 32))
         ((= len 41)
          (ffi-get-c-string buf))
         (t (error "Unsupported value length.")))))
     (t (error "Socket option not handled yet.")))))

(provide 'zmq-ffi)

;; Local Variables:
;; byte-compile-warnings: (not unresolved)
;; End:
