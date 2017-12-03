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

(define-ffi-array zmq-msg :uchar 64)

(defconst zmq--work-buffer-size 256)
(defconst zmq--work-buffer (ffi-allocate zmq--work-buffer-size)
  "A buffer used internally by `zmq'

See `zmq-getsockopt'.")

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
              (string-bindings (cl-loop
                                for i from 0 to (1- (length arg-types))
                                when (eq (aref arg-types i) :string) do
                                (aset arg-types i :pointer) and
                                collect (let ((arg (nth i args)))
                                          (list arg arg))))
              (body `(let ((ret (,ffi-name ,@args)))
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

;; TODO: Better way to handle this
;; http://zguide.zeromq.org/page:all#A-Minor-Note-on-Strings
(defun zmq--get-bytes (buf len)
  "Get the bytes in BUF as a multibyte string.

The size of BUF must be at least 1+LEN. This is because BUF[LEN]
is set to 0 in order to NULL terminate the string. See
http://phst.github.io/emacs-modules.html#caveats-and-bugs"
  ;; NOTE: that in elisp, strings can contain NULL characters The current
  ;; implementation of the ffi library doesn't handle binary data in a
  ;; symmetric way. `ffi-get-c-string' uses `strlen', which for binary data may
  ;; not give expected results since `strlen' looks for the NULL byte to
  ;; indicate the end of a string. `ffi-make-c-string' uses
  ;; `copy_string_contents' from the emacs module which handles NULL bytes in
  ;; an elisp string properly.
  ;;
  ;; TODO: For large amounts of binary data, it would be best to offload the
  ;; work to the ffi c-module by instantiating a vector and setting the bytes.
  ;; TODO: Added a new `ffi-get-bytes' function.
  ;; Chop off the message and NULL terminate if larger than the buffer size.
  (ffi--mem-set (ffi-pointer+ buf len) :char 0)
  (ffi-get-bytes buf len))

;; TODO: Implement an (ffi-set-bytes pointer string len) which copies
;; lisp string to memory pointed to by pointer with len size.
;; TODO: Need to convert to unibyte representation?
(defun zmq--set-bytes (buf data)
  "Set the contents of BUF to the string DATA.
BUF must be large enough to hold (1+ (length data)) of bytes. If
optional argument NONULL is non-nil, then a NULL byte is not set
at the next buffer position after copying DATA. This means that
BUF may have size (length data)."
  (cl-loop for i from 0 to (1- (length data))
           do (ffi--mem-set (ffi-pointer+ zmq--work-buffer i)
                            :char (aref data i)))
  (ffi--mem-set buf
                (ffi-pointer+ buf (length data)
                              :char 0)))

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
  :int ((message :pointer) (socket :pointer) (flags :int)))
(zmq--ffi-function-wrapper "msg_routing_id" :uint32 ((message :pointer)))
(zmq--ffi-function-wrapper "msg_send"
  :int ((message :pointer) (socket :pointer) (flags :int)))
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

(defun zmq-recv-multipart (socket)
  (let ((res '()))
    (with-zmq-msg part nil
      (catch 'done
        (while t
          (zmq-msg-init part)
          (zmq-msg-recv part socket 0)
          (push (zmq-msg-data part) res)
          (unless (zmq-msg-more part)
            (throw 'done (mapconcat 'identity (nreverse res) ""))))))))

;;; Polling

(zmq--ffi-function-wrapper "poll" :int [:pointer :int :long] internal)

(defun zmq-pollitem-new (&rest args)
  (let ((item (ffi-allocate zmq-pollitem))
        (val nil))
    (setf (zmq-pollitem-socket item) (if (setq val (plist-get args :socket))
                                         val
                                       (ffi-null-pointer)))
    (setf (zmq-pollitem-fd item) (if (setq val (plist-get args :fd))
                                     val
                                   -1))
    (setf (zmq-pollitem-fd item) (if (setq val (plist-get args :events))
                                     val
                                   0))
    (setf (zmq-pollitem-fd item) (if (setq val (plist-get args :revents))
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
  :int ((socket :pointer) (endpoint :string) (events :int)))

(zmq--ffi-function-wrapper "bind" :int ((socket :pointer) (endpoint :string)))
(zmq--ffi-function-wrapper "unbind" :int ((socket :pointer) (endpoint :string)))

(zmq--ffi-function-wrapper "getsockopt"
  :int ((socket :pointer) (option :int) (value :pointer) (len :pointer)) internal)
(zmq--ffi-function-wrapper "setsockopt"
  :int [:pointer :int :pointer :size_t] internal)

(zmq--ffi-function-wrapper "connect"
  :int ((socket :pointer) (endpoint :string)))
(zmq--ffi-function-wrapper "disconnect"
  :int ((socket :pointer) (endpoint :string)))

(zmq--ffi-function-wrapper "close" :int ((socket :pointer)))

(defun zmq-send-const (sock buf len &optional flags)
  (when (cl-assert (user-ptrp buf))
    (zmq--send-const sock buf len (or flags 0))))

(defun zmq-send (sock buf &optional flags)
  (with-ffi-string (_buf buf)
    (zmq--send sock _buf (length buf) (or flags 0))))

(defun zmq-recv (sock buf len &optional flags)
  (when (or (null buf) (cl-assert (user-ptrp buf)))
    (let ((allocated (null buf))
          (buf (or buf (ffi-allocate len))))
      (unwind-protect
          (let ((rlen (zmq--recv sock buf len (or flags 0))))
            (when (> rlen (1- len))
              (setq rlen (1- len)))
            (zmq--get-bytes buf rlen))
        (when allocated
          (ffi-free buf))))))

(defun zmq-setsockopt (socket option value)
  (let (size c-val)
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
      (ffi--mem-set zmq--work-buffer :int value))
     ;; UINT64
     ((member option (list zmq-AFFINITY zmq-VMCI_BUFFER_SIZE
                           zmq-VMCI_BUFFER_MAX_SIZE zmq-VMCI_BUFFER_MIN_SIZE))
      (setq size (ffi--type-size :uint64))
      (ffi--mem-set zmq--work-buffer :uint64 value))
     ;; INT64
     ((member option (list zmq-MAXMSGSIZE))
      (setq size (ffi--type-size :int64))
      (ffi--mem-set zmq--work-buffer :int64 value))
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
      (ffi--mem-set zmq--work-buffer :int (if value 1 0)))
     ;; STRING
     ((member option (list zmq-SUBSCRIBE zmq-GSSAPI_PRINCIPAL
                           zmq-GSSAPI_SERVICE_PRINCIPAL zmq-PLAIN_PASSWORD
                           zmq-PLAIN_USERNAME zmq-SOCKS_PROXY zmq-ZAP_DOMAIN))
      (setq size (1+ (length value)))
      (unless (< size zmq--work-buffer-size)
        (error "Length of value too long."))
      (zmq--set-bytes zmq--work-buffer value))
     ;; BINARY
     ((member option (list zmq-CONNECT_ROUTING_ID zmq-ROUTING_ID zmq-SUBSCRIBE
                           zmq-UNSUBSCRIBE zmq-XPUB_WELCOME_MSG))
      (setq size (length value))
      ;; account for extra null byte added in `zmq--set-bytes'
      (unless (< (1+ size) zmq--work-buffer-size)
        (error "Length of value too long."))
      (zmq--set-bytes zmq--work-buffer value))
     ;; CURVE
     ((member option (list zmq-CURVE_PUBLICKEY zmq-CURVE_SECRETKEY
                           zmq-CURVE_SERVERKEY))
      (cond
       ((= (length value) 32)
        ;; don't copy the NULL byte
        (setq size 32)
        (zmq--set-bytes zmq--work-buffer value))
       ((= (length value) 40)
        (setq size 41)
        (zmq--set-bytes zmq--work-buffer value))
       (t (error "Unsupported value length."))))
     (t (error "Socket option not handled yet.")))
    (zmq--setsockopt socket option c-val size)))

(defun zmq-getsockopt (socket option)
  (let ((value zmq--work-buffer))
    (with-ffi-temporaries ((len :size_t))
      (ffi--mem-set len :size_t zmq--work-buffer-size)
      (zmq--getsockopt socket option value len)
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
        (ffi--mem-ref value :int))
       ;; UINT64
       ((member option (list zmq-AFFINITY zmq-VMCI_BUFFER_SIZE
                             zmq-VMCI_BUFFER_MIN_SIZE
                             zmq-VMCI_BUFFER_MAX_SIZE))
        (ffi--mem-ref value :uint64))
       ;; INT64
       ((= option zmq-MAXMSGSIZE)
        (ffi--mem-ref value :int64))
       ;; TODO: Different on windows
       ((= option zmq-FD)
        (ffi--mem-ref value :int))
       ;; INT with BOOL values
       ((member option (list zmq-IMMEDIATE zmq-INVERT_MATCHING
                             zmq-IPV6 zmq-GSSAPI_PLAINTEXT
                             zmq-GSSAPI_SERVER
                             zmq-RCVMORE))
        (= (ffi--mem-ref value :int) 1))
       ;; BOOL
       ((= option zmq-THREAD_SAFE)
        (ffi--mem-ref value :bool))
       ;; STRINGS
       ((member option (list zmq-GSSAPI_PRINCIPAL zmq-GSSAPI_SERVICE_PRINCIPAL
                             zmq-LAST_ENDPOINT zmq-PLAIN_PASSWORD
                             zmq-PLAIN_USERNAME zmq-SOCKS_PROXY
                             zmq-ZAP_DOMAIN))
        (ffi-get-c-string value))
       ;; CURVE
       ((member option (list zmq-CURVE_PUBLICKEY zmq-CURVE_SECRETKEY
                             zmq-CURVE_SERVERKEY))
        (let ((len (ffi--mem-ref len :size_t)))
          (cond
           ;; TODO: What happens when the value is NULL (the default)? Does
           ;; zmq--getsockopt error out in that case?
           ((= len 0) nil)
           ((= len 32)
            (zmq--get-bytes value 32))
           ((= len 41)
            (ffi-get-c-string value))
           (t (error "Unsupported value length.")))))
       (t (error "Socket option not handled yet."))))))

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
