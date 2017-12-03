(require 'ffi)
(require 'zmq-constants)
(require 'cl-lib)

(define-ffi-library libzmq "libzmq")

;;; Types

;; TODO: The fd field is different on windows
(define-ffi-struct zmq-pollitem
  (socket :type :pointer)
  (fd :type :int)
  (events :type :short)
  (revents :type :short))
(define-ffi-array zmq-msg :char 64)
(define-ffi-array zmq-work-buffer :char 256)

(defconst zmq--buf (let ((buf (ffi-allocate zmq-work-buffer)))
                     (dotimes (i (ffi--type-size zmq-work-buffer))
                       (ffi--mem-set (ffi-pointer+ buf i) :char 0))
                     buf)
  "A buffer used internally by `zmq'

See `zmq-getsockopt' and `zmq-set-sockopt'.")

;;; FFI wrapper

;; Flag value:
;;
;; TODO: Deconvolute the flag parameter
(defmacro zmq--ffi-function-wrapper (c-name return-type arg-types &optional flag noerror)
  (declare (debug t) (indent 1))
  (when (and (consp flag) (eq (car flag) 'quote))
    (setq flag (cdr flag)))
  (let* ((fname (subst-char-in-string ?_ ?- c-name))
         (c-name (concat "zmq_" c-name))
         (ffi-name (intern (cond
                            ((eq flag 'internal) (concat "zmq--" fname "-1"))
                            ((eq flag 'nowrap) (concat "zmq-" fname))
                            (t (concat "zmq--" fname))))))
    (macroexpand-all
     (if (eq flag 'nowrap)
         `(define-ffi-function
            ,ffi-name ,c-name ,return-type ,arg-types zmq)
       (let* ((wrapped-name (intern (cond
                                     ((eq flag 'internal) (concat "zmq--" fname))
                                     (t (concat "zmq-" fname)))))
              (args (if (listp arg-types)
                        (cl-loop
                         for (name type) in arg-types
                         collect name and collect type into types
                         finally do (setq arg-types (apply #'vector types)))
                      (cl-loop repeat (length arg-types)
                               collect (cl-gensym))))
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
              (string-bindings
               (cl-loop
                for i from 0 to (1- (length arg-types))
                when (eq (aref arg-types i) :string)
                do (aset arg-types i :pointer)
                and collect (let ((arg (nth i args)))
                              (list arg `(encode-coding-string ,arg 'utf-8)))))
              (body `(let ((ret (,ffi-name ,@args)))
                       ;; Depending on the return-type, check if an error is
                       ;; set.
                       (if ,(cl-case return-type
                              (:pointer '(ffi-pointer-null-p ret))
                              (:int '(= ret -1))
                              ;; Just return without checking for errors
                              (t nil))
                           ,(if noerror 'nil '(zmq-error-handler))
                         ret))))
         `(progn
            (define-ffi-function ,ffi-name ,c-name ,return-type ,arg-types libzmq)
            (defun ,wrapped-name ,args
              ,(if string-bindings
                   `(with-ffi-strings ,string-bindings
                      ,body)
                 body))))))))

;;; Memory handling functions

(defun zmq--get-bytes (buf size)
  "Get SIZE bytes of BUF and return a `string'."
  (let ((i size) data)
    (while (>= (setq i (1- i)) 0)
      (setq data (cons (ffi--mem-ref (ffi-pointer+ buf i) :char) data)))
    (apply #'string data)))

(defun zmq--set-bytes (buf data)
  "Set the contents of BUF to DATA.

DATA must be a string and BUF must be a pointer to memory which
can hold at least (1+ (string-bytes DATA)) of bytes.

After copying DATA, an extra NULL byte is added at
BUF[(string-bytes DATA)]. This is so that BUF contains a valid
c-string if DATA has no NULL bytes and also a valid elisp string
if DATA contains NULL bytes. Both types of strings require a
terminating NULL. Note the distinction between c-strings and
elisp strings, if DATA has NULL bytes then it is not a valid
c-string and you will get the wrong data if you attempt to call
`ffi-make-c-string' on BUF after setting its bytes."
  (unless (stringp data)
    (error "Data can only be a string."))
  (setq data (encode-coding-string data 'utf-8))
  ;; data is now a `unibyte-string'
  (dotimes (i (length data))
    (ffi--mem-set (ffi-pointer+ buf i) :char (aref data i)))
  ;; NOTE: http://zguide.zeromq.org/page:all#A-Minor-Note-on-Strings
  ;; Also note that
  ;; Assume BUF can hold an extra byte
  (ffi--mem-set (ffi-pointer+ buf (length data)) :char 0))

(defun zmq--set-buf (type-or-value &optional value)
  "Set `zmq--buf' depending on TYPE-OR-VALUE and VALUE.

If TYPE-OR-VALUE is non-nil, then it can only be one of the
primitive types such as `:int' or `:char' and VALUE must be
non-nil and should have the type corresponding to TYPE-OR-VALUE.

If VALUE is nil, then TYPE-OR-VALUE is assumed to be a string
which is used to set `zmq--buf'."
  (cond
   ((and (null value) (stringp type-or-value))
    (setq value type-or-value)
    (zmq--set-bytes zmq--buf value))
   ((and value (keywordp type-or-value))
    (ffi--mem-set zmq--buf type-or-value value))
   (t (signal 'wrong-type-argument nil))))

;;; Error handling

;; These are used in `zmq--ffi-function-wrapper' so don't try to wrap them.
(define-ffi-function zmq-errno "zmq_errno" :int [] libzmq)
(define-ffi-function zmq-strerror "zmq_strerror" :pointer [:int] libzmq)

(defun zmq-error-handler ()
  (let* ((errno (zmq-errno))
         (err (assoc errno zmq-error-alist)))
    (signal (or (cdr err) 'zmq-ERROR)
            (ffi-get-c-string (zmq-strerror errno)))))

;;; Contexts

(zmq--ffi-function-wrapper "ctx_new" :pointer [] nil noerror)
(zmq--ffi-function-wrapper "ctx_set"
  :int ((context :pointer) (option :int) (value :int)))
(zmq--ffi-function-wrapper "ctx_get" :int ((context :pointer) (option :int)))
(zmq--ffi-function-wrapper "ctx_term" :int ((context :pointer)))
(zmq--ffi-function-wrapper "ctx_shutdown" :int ((context :pointer)))

;;; Encryption

(zmq--ffi-function-wrapper "z85_decode"
  :pointer [:pointer :pointer] internal noerror)
(zmq--ffi-function-wrapper "z85_encode"
  :pointer [:pointer :pointer :size_t] internal noerror)
(zmq--ffi-function-wrapper "curve_keypair" :int [:pointer :pointer] internal)
(zmq--ffi-function-wrapper "curve_public" :int [:pointer :pointer] internal)

(defun zmq-z85-decode (data)
  (with-ffi-string (_data data)
    (with-ffi-temporary (decoded (ceiling (* 0.8 (length data))))
      (when (ffi-pointer= decoded (zmq--z85-decode decoded _data))
        (ffi-get-c-string decoded)))))

(defun zmq-z85-encode (data)
  (with-ffi-string (_data data)
    (with-ffi-temporary (encoded (+ (ceiling (* 1.25 (length data))) 1))
      (when (ffi-pointer= encoded (zmq--z85-encode
                                   encoded _data (length data)))
        (ffi-get-c-string encoded)))))

(defun zmq-curve-keypair ()
  (unless (zmq-has "curve")
    (error "0MQ not built with CURVE security"))
  (with-ffi-temporaries ((public-key 41)
                         (secret-key 41))
    (zmq--curve-keypair public-key secret-key)
    (cons (ffi-get-c-string public-key)
          (ffi-get-c-string secret-key))))

(defun zmq-curve-public (secret-key)
  (unless (zmq-has "curve")
    (error "0MQ not built with CURVE security"))
  (with-ffi-string (secret-key secret-key)
    (with-ffi-temporary (public-key 41)
      (zmq--curve-public public-key secret-key)
      (ffi-get-c-string public-key))))

;;; Messages

(zmq--ffi-function-wrapper "msg_close" :int [:pointer] internal)
(zmq--ffi-function-wrapper "msg_copy" :int [:pointer :pointer] internal)
(zmq--ffi-function-wrapper "msg_data" :pointer [:pointer] internal noerror)
(zmq--ffi-function-wrapper "msg_gets" :pointer [:pointer :pointer] internal)
(zmq--ffi-function-wrapper "msg_get" :int ((message :pointer) (property :int)))
(zmq--ffi-function-wrapper "msg_init_data"
  :int [:pointer :pointer :size_t :pointer :pointer] internal)
(zmq--ffi-function-wrapper "msg_init_size"
  :int ((message :pointer) (size :size_t)))
(zmq--ffi-function-wrapper "msg_init" :int ((message :pointer)))
(zmq--ffi-function-wrapper "msg_more" :int [:pointer] internal)
(zmq--ffi-function-wrapper "msg_move" :int ((dest :pointer) (src :pointer)))
(zmq--ffi-function-wrapper "msg_recv"
  :int ((message :pointer) (sock :pointer) (flags :int)))
(zmq--ffi-function-wrapper "msg_routing_id" :uint32 ((message :pointer)))
(zmq--ffi-function-wrapper "msg_send"
  :int ((message :pointer) (sock :pointer) (flags :int)))
(zmq--ffi-function-wrapper "msg_set_routing_id"
  :int ((message :pointer) (routing-id :int)))
(zmq--ffi-function-wrapper "msg_set"
  :int ((message :pointer) (property :int) (val :int)))
(zmq--ffi-function-wrapper "msg_size" :size_t ((message :pointer)))

(defun zmq--msg-init-data-free (data hint)
  (ffi-free data))

(defvar zmq--msg-init-data-free-fn
  (ffi-make-closure (ffi--prep-cif :void [:pointer :pointer])
                    'zmq--msg-init-data-free)
  "Function pointer for use in `zmq-msg-init-data'.")

(defun zmq-msg-new (&optional init-val)
  "Create a new message, and optionally initialize it.

If INIT-VAL is a string, initialize the message with the string
data. If INIT-VAL is an integer, initialize the message to be
INIT-VAL in size. If INIT-VAL is t, initialize an empty message.
Otherwise just create a message without initializing it."
  (let ((msg (ffi-allocate zmq-msg)))
    (cond
     ((stringp init-val) (zmq-msg-init-data msg init-val))
     ((integerp init-val) (zmq-msg-init-size msg init-val))
     ((eq init-val t) (zmq-msg-init msg)))
    msg))

(defun zmq-msg-close (message)
  (unwind-protect
      (zmq--msg-close message)
    (ffi-free message)))

(defun zmq-msg-copy (message)
  (let ((dest (zmq-msg-new)))
    (condition-case nil
        (progn
          (zmq--msg-copy dest message)
          dest)
      (error (zmq-msg-close dest)))))

(defun zmq-msg-data (message)
  (let ((data (zmq--msg-data message)))
    (when data
      (zmq--get-bytes data (zmq-msg-size message)))))

(defun zmq-msg-gets (message property)
  (with-ffi-string (prop (encode-coding-string property 'utf-8))
    (encode-coding-string
     (ffi-get-c-string (zmq--msg-gets message prop)) 'utf-8)))

(defun zmq-msg-init-data (message data)
  (let ((buf (ffi-make-c-string data)))
    (zmq--msg-init-data
     message buf (length data)
     zmq--msg-init-data-free-fn (ffi-null-pointer))
    message))

(defun zmq-msg-more (message)
  (= (zmq--msg-more message) 1))

;;; Polling

(zmq--ffi-function-wrapper "poll" :int [:pointer :int :long] internal)

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

(zmq--ffi-function-wrapper "proxy_steerable"
  :int ((frontend :pointer) (backend :pointer)
        (capture :pointer) (control :pointer)))
(zmq--ffi-function-wrapper "proxy"
  :int ((frontend :pointer) (backend :pointer) (capture :pointer)))

;;; Sockets

(zmq--ffi-function-wrapper "send_const"
  :int [:pointer :pointer :size_t :int] internal)
(zmq--ffi-function-wrapper "send"
  :int [:pointer :pointer :size_t :int] internal)
(zmq--ffi-function-wrapper "recv"
  :int [:pointer :pointer :size_t :int] internal)

(zmq--ffi-function-wrapper "socket" :pointer ((context :pointer) (type :int)))
(zmq--ffi-function-wrapper "socket_monitor"
  :int ((sock :pointer) (endpoint :string) (events :int)))

(zmq--ffi-function-wrapper "bind" :int ((sock :pointer) (endpoint :string)))
(zmq--ffi-function-wrapper "unbind" :int ((sock :pointer) (endpoint :string)))

(zmq--ffi-function-wrapper "getsockopt"
  :int ((sock :pointer) (option :int) (value :pointer) (len :pointer)) internal)
(zmq--ffi-function-wrapper "setsockopt"
  :int [:pointer :int :pointer :size_t] internal)

(zmq--ffi-function-wrapper "connect"
  :int ((sock :pointer) (endpoint :string)))
(zmq--ffi-function-wrapper "disconnect"
  :int ((sock :pointer) (endpoint :string)))

(zmq--ffi-function-wrapper "close" :int ((sock :pointer)))

(defun zmq-send-const (sock buf len &optional flags)
  (when (cl-assert (user-ptrp buf))
    (zmq--send-const sock buf len (or flags 0))))

(defun zmq-send (sock message &optional flags)
  "Send a message on SOCK."
  (let ((msg (zmq-msg-new message)))
    (unwind-protect
        (zmq-msg-send msg sock (or flags 0))
      (zmq-msg-close msg))))

(defun zmq-send-multipart (sock parts &optional flags)
  "Send a multipart message with PARTS on SOCK with FLAGS."
  (let ((msg (zmq-msg-new t)))
    (unwind-protect
        (while (car parts)
          (zmq-msg-init-data msg (car parts))
          (setq parts (cdr parts))
          (when parts
            (zmq-msg-set msg zmq-MORE 1))
          (zmq-msg-send msg sock (or flags 0)))
      (zmq-msg-close msg))))

(defun zmq-recv (sock &optional flags)
  "Receive a message from SOCK with FLAGS."
  (let ((msg (zmq-msg-new)))
    (unwind-protect
        (progn
          (zmq-msg-recv msg sock (or flags 0))
          (zmq-msg-data msg))
      (zmq-msg-close msg))))

(defun zmq-recv-multipart (sock)
  "Receive a multipart message from SOCK."
  (let (res)
    (let ((part (zmq-msg-new)))
      (unwind-protect
          (catch 'recvd
            (while t
              (zmq-msg-init part)
              (zmq-msg-recv part sock 0)
              (setq res (cons (zmq-msg-data part) res))
              (unless (zmq-msg-more part)
                (throw 'recvd (nreverse res)))))
        (zmq-msg-close part)))))

(defun zmq-setsockopt (sock option value)
  (let (size)
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
      (setq size (ffi--type-size :int))
      (zmq--set-buf :int value))
     ;; UINT64
     ((member option (list zmq-AFFINITY zmq-VMCI_BUFFER_SIZE
                           zmq-VMCI_BUFFER_MAX_SIZE zmq-VMCI_BUFFER_MIN_SIZE))
      (setq size (ffi--type-size :uint64))
      (zmq--set-buf :uint64 value))
     ;; INT64
     ((member option (list zmq-MAXMSGSIZE))
      (setq size (ffi--type-size :int64))
      (zmq--set-buf :int64 value))
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
      (setq size (ffi--type-size :int))
      (zmq--set-buf :int64 (if value 1 0)))
     ;; STRING
     ((member option (list zmq-SUBSCRIBE zmq-GSSAPI_PRINCIPAL
                           zmq-GSSAPI_SERVICE_PRINCIPAL zmq-PLAIN_PASSWORD
                           zmq-PLAIN_USERNAME zmq-SOCKS_PROXY zmq-ZAP_DOMAIN))
      (setq size (1+ (length value)))
      (unless (< size (ffi--type-size zmq-work-buffer))
        (error "Length of value too long."))
      (zmq--set-buf value))
     ;; BINARY
     ((member option (list zmq-CONNECT_ROUTING_ID zmq-ROUTING_ID zmq-SUBSCRIBE
                           zmq-UNSUBSCRIBE zmq-XPUB_WELCOME_MSG))
      (setq size (length value))
      ;; account for extra null byte added in `zmq--set-bytes'
      (unless (< (1+ size) (ffi--type-size zmq-work-buffer))
        (error "Length of value too long."))
      (zmq--set-buf value))
     ;; CURVE
     ((member option (list zmq-CURVE_PUBLICKEY zmq-CURVE_SECRETKEY
                           zmq-CURVE_SERVERKEY))
      (cond
       ((= (length value) 32)
        ;; don't copy the NULL byte
        (setq size 32)
        (zmq--set-buf value))
       ((= (length value) 40)
        (setq size 41)
        (zmq--set-buf value))
       (t (error "Unsupported value length."))))
     (t (error "Socket option not handled yet.")))
    (zmq--setsockopt sock option zmq--buf size)))

(defun zmq-getsockopt (sock option)
  (with-ffi-temporaries ((len :size_t))
    (ffi--mem-set len :size_t (ffi--type-size zmq-work-buffer))
    (zmq--getsockopt sock option zmq--buf len)
    (cond
     ;; INT
     ((member option (list zmq-HANDSHAKE_IVL zmq-BACKLOG
                           zmq-CONNECT_TIMEOUT zmq-EVENTS
                           zmq-ROUTING_ID zmq-LINGER zmq-MECHANISM
                           zmq-MULTICAST_HOPS zmq-MULTICAST_MAXTPDU
                           zmq-PLAIN_SERVER zmq-USE_FD zmq-RATE zmq-RCVBUF
                           zmq-RCVHWM zmq-RCVTIMEO zmq-RECONNECT_IVL
                           zmq-RECONNECT_IVL_MAX zmq-RECOVERY_IVL
                           zmq-SNDBUF zmq-SNDHWM zmq-SNDTIMEO
                           zmq-TCP_KEEPALIVE zmq-TCP_KEEPALIVE_CNT
                           zmq-TCP_KEEPALIVE_IDLE zmq-TCP_KEEPALIVE_INTVL
                           zmq-TCP_MAXRT zmq-TOS zmq-TYPE
                           zmq-VMCI_CONNECT_TIMEOUT))
      (ffi--mem-ref zmq--buf :int))
     ;; UINT64
     ((member option (list zmq-AFFINITY zmq-VMCI_BUFFER_SIZE
                           zmq-VMCI_BUFFER_MIN_SIZE
                           zmq-VMCI_BUFFER_MAX_SIZE))
      (ffi--mem-ref zmq--buf :uint64))
     ;; INT64
     ((= option zmq-MAXMSGSIZE)
      (ffi--mem-ref zmq--buf  :int64))
     ;; TODO: Different on windows
     ((= option zmq-FD)
      (ffi--mem-ref zmq--buf  :int))
     ;; INT with BOOL values
     ((member option (list zmq-IMMEDIATE zmq-INVERT_MATCHING
                           zmq-IPV6 zmq-GSSAPI_PLAINTEXT
                           zmq-GSSAPI_SERVER
                           zmq-RCVMORE))
      (= (ffi--mem-ref zmq--buf  :int) 1))
     ;; BOOL
     ((= option zmq-THREAD_SAFE)
      (ffi--mem-ref zmq--buf  :bool))
     ;; STRINGS
     ((member option (list zmq-GSSAPI_PRINCIPAL zmq-GSSAPI_SERVICE_PRINCIPAL
                           zmq-LAST_ENDPOINT zmq-PLAIN_PASSWORD
                           zmq-PLAIN_USERNAME zmq-SOCKS_PROXY
                           zmq-ZAP_DOMAIN))
      (ffi-get-c-string zmq--buf))
     ;; CURVE
     ((member option (list zmq-CURVE_PUBLICKEY zmq-CURVE_SECRETKEY
                           zmq-CURVE_SERVERKEY))
      (let ((len (ffi--mem-ref len :size_t)))
        (cond
         ;; TODO: What happens when the value is NULL (the default)? Does
         ;; zmq--getsockopt error out in that case?
         ((= len 0) nil)
         ((= len 32)
          (zmq--get-bytes zmq--buf 32))
         ((= len 41)
          (ffi-get-c-string zmq--buf))
         (t (error "Unsupported value length.")))))
     (t (error "Socket option not handled yet.")))))

;;; Utility

(zmq--ffi-function-wrapper "has" :int [:pointer] internal)
(zmq--ffi-function-wrapper "version"
  :void [:pointer :pointer :pointer] internal)

(defun zmq-has (capability)
  (with-ffi-string (capability capability)
    (= (zmq--has capability) 1)))

(defun zmq-version ()
  (with-ffi-temporaries ((major :int)
                         (minor :int)
                         (patch :int))
    (zmq--version major minor patch)
    (mapconcat (lambda (x) (number-to-string (ffi--mem-ref x :int)))
               (list major minor patch)
               ".")))

(provide 'zmq-ffi)

;; Local Variables:
;; byte-compile-warnings: (not unresolved)
;; End:
