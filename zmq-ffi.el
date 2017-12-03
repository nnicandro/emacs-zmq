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
                           ,(if noerror 'nil
                              '(error (ffi-get-c-string
                                       (zmq-strerror (zmq-errno)))))
                         ret))))
         `(progn
            (define-ffi-function ,ffi-name ,c-name ,return-type ,arg-types libzmq)
            (defun ,wrapped-name ,args
              ,(if string-bindings
                   `(with-ffi-strings ,string-bindings
                      ,body)
                 body))))))))

;;; Error handling

;; These are used in `zmq--ffi-function-wrapper' so don't try to wrap them.

(define-ffi-function zmq-errno "zmq_errno" :int [] libzmq)
(define-ffi-function zmq-strerror "zmq_strerror" :pointer [:int] libzmq)

;;; Contexts

(zmq--ffi-function-wrapper "ctx_new" :pointer [] nil noerror)
(zmq--ffi-function-wrapper "ctx_set"
  :int ((context :pointer) (option :int) (val :int)))
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
    (with-ffi-temporary (decoded (ceiling (* 0.8 (string-bytes data))))
      (when (ffi-pointer= decoded (zmq--z85-decode decoded _data))
        (ffi-get-c-string decoded)))))

(defun zmq-z85-encode (data)
  (with-ffi-string (_data data)
    (with-ffi-temporary (encoded (+ (ceiling (* 1.25 (string-bytes data))) 1))
      (when (ffi-pointer= encoded (zmq--z85-encode
                                   encoded _data (string-bytes data)))
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
  ;; TODO: Thread safety? How will this function be called in emacs?
  (ffi-free data))

(defvar zmq--msg-init-data-free-fn
  (ffi-make-closure (ffi--prep-cif :void [:pointer :pointer]) 'zmq--msg-init-data-free)
  "Function pointer for use in `zmq-msg-init-data'.")

(defun zmq-msg-new ()
  (ffi-allocate zmq-msg))

;; Actual message API

(defun zmq-msg-close (message)
  (unwind-protect
      (zmq--msg-close message)
    ;; TODO: Is freeing this OK after a close? The above function doesn't
    ;; actually release the resources of message.
    (ffi-free message)))

(defun zmq-msg-copy (message)
  (let ((dest (zmq-msg-new)))
    (condition-case nil
        (progn
          (zmq--msg-copy dest message)
          dest)
      (error (ffi-free dest)))))

(defun zmq-msg-data (message)
  (let ((data (zmq--msg-data message)))
    (when data
      (if (= (ffi--mem-ref (ffi-pointer+ data (1- (zmq-msg-size message)))
                           :uint8)
             0)
          ;; Assume the message data is a NULL terminated string
          (ffi-get-c-string data)
        ;; Convert to a binary representation otherwise
        (apply #'unibyte-string
               (cl-loop
                for i from 0 to (1- (zmq-msg-size message))
                collect (ffi--mem-ref (ffi-pointer+ data i)
                                      :unit8)))))))

(defun zmq-msg-gets (message property)
  (with-ffi-string (prop (encode-coding-string property 'utf-8))
    (encode-coding-string
     (ffi-get-c-string (zmq--msg-gets message prop)) 'utf-8)))

(defun zmq-msg-init-data (data)
  (let* ((message (zmq-msg-new))
         (buf (ffi-make-c-string data)))
    (zmq--msg-init-data
     message buf (string-bytes data)
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

;; TODO: Internalize this
(zmq--ffi-function-wrapper "getsockopt"
  :int ((socket :pointer) (option :int) (value :pointer) (len :pointer)))
(zmq--ffi-function-wrapper "setsockopt"
  :int [:pointer :int :pointer :size_t] internal)

(zmq--ffi-function-wrapper "connect"
  :int ((socket :pointer) (endpoint :string)))
(zmq--ffi-function-wrapper "disconnect"
  :int ((socket :pointer) (endpoint :string)))

(zmq--ffi-function-wrapper "close" :int ((socket :pointer)))

(defun zmq-send-const (sock buf len &optional flags)
  (cl-assert (user-ptrp buf))
  (zmq--send-const sock buf len (or flags 0)))

(defun zmq-send (sock buf &optional flags)
  (with-ffi-string (_buf buf)
    (zmq--send sock _buf (string-bytes buf) (or flags 0))))

(defun zmq-recv (sock len &optional flags buf)
  (unless (null buf) (cl-assert (user-ptrp buf)))
  (let ((buf (or buf (ffi-allocate len)))
        (allocated (null buf)))
    (zmq--recv sock buf len (or flags 0))
    (prog1 (ffi-get-c-string buf)
      (when allocated
        (ffi-free buf)))))

(defun zmq-setsockopt (sock opt val)
  (let (size c-val)
    (cond
     ((= opt zmq-SUBSCRIBE)
      (setq size (string-bytes val)
            c-val (ffi-make-c-string val)))
     ((= opt zmq-ROUTING_ID)
      ;; Binary data between 1 and 255 bytes long
      (setq size (string-bytes val)
            c-val (ffi-make-c-string val)))
     ((= opt zmq-LINGER)
      (setq size (ffi--type-size :int)
            c-val (let ((c-val (ffi-allocate :int)))
                    (ffi--mem-set c-val :int val)
                    c-val)))
     (t (error "Socket option not handled yet.")))
    (unwind-protect
        (zmq--setsockopt sock opt c-val size)
      (ffi-free c-val))))

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
