(require 'cl-lib)
(require 'ert)
(require 'zmq)

(defun zmq-create-bound-pair (ctx type1 type2 &optional interface)
  (setq interface (or interface "tcp://127.0.0.1"))
  (let ((s1 (zmq-socket ctx type1))
        (s2 (zmq-socket ctx type2)))
    (zmq-set-option s1 zmq-LINGER 0)
    (zmq-set-option s2 zmq-LINGER 0)
    (let ((port (zmq-bind-to-random-port s1 interface)))
      (zmq-connect s2 (format "%s:%d" interface port)))
    (cons s1 s2)))

(ert-deftest zmq-utility ()
  (ert-info ("`zmq-version'")
    (let ((version (zmq-version)))
      (should (version<= "4.0.0" version))))
  (ert-info ("`zmq-has'")
    (should (or (eq (zmq-has "draft") t)
                (eq (zmq-has "draft") nil))))
  (with-zmq-context
    (ert-info ("Convenience functions")
      (ert-info ("`zmq-bind-to-random-port'")
        (with-zmq-socket sock zmq-PUB
          (should-error
           (zmq-bind-to-random-port sock "tcp:*")
           :type 'zmq-EINVAL)
          (should-error
           (zmq-bind-to-random-port sock "rand://")
           :type 'zmq-EPROTONOSUPPORT))))))

(ert-deftest zmq-encryption ()
  (ert-info ("CURVE mechanism")
    (when (zmq-has "curve")
      (cl-destructuring-bind (public-key . secret-key) (zmq-curve-keypair)
        (should (string= (zmq-z85-decode (zmq-z85-encode public-key))
                         public-key))
        (should (string= public-key (zmq-curve-public secret-key)))))))

(ert-deftest zmq-contexts ()
  (let (ctx)
    (ert-info ("Creating contexts")
      (setq ctx (zmq-context)))
    (ert-info ("Setting/getting context options")
      (zmq-set-option ctx zmq-MAX_MSGSZ 100)
      (should (= (zmq-get-option ctx zmq-MAX_MSGSZ) 100))
      (zmq-context-set ctx zmq-MAX_MSGSZ 200)
      (should (= (zmq-context-get ctx zmq-MAX_MSGSZ) 200))
      ;; BOOL options
      (zmq-context-set ctx zmq-BLOCKY nil)
      (should-not (zmq-context-get ctx zmq-BLOCKY))
      (zmq-context-set ctx zmq-BLOCKY t)
      (should (eq (zmq-context-get ctx zmq-BLOCKY) t))
      (should-error (zmq-context-get ctx "foo")
                    :type 'wrong-type-argument)
      (should-error (zmq-context-get "bar" zmq-MAX_MSGSZ)
                    :type 'wrong-type-argument))
    (ert-info ("Context termination")
      (zmq-terminate-context ctx)
      (should-error (zmq-terminate-context ctx)
                    :type 'zmq-EFAULT))
    (ert-info ("Context macro")
      (let (ctx)
        (with-zmq-context
          (setq ctx (zmq-current-context))
          (should (zmq-context-p ctx)))
        (should-error (zmq-terminate-context ctx)
                      :type 'zmq-EFAULT)))))

(ert-deftest zmq-messages ()
  (ert-info ("Message initialization")
    (let ((msg (zmq-message)) contents)
      (unwind-protect
          (progn
            (ert-info ("Empty constructor")
              (should (= (zmq-message-size msg) 0))
              (should (equal (zmq-message-data msg) ""))
              (zmq-close-message msg))
            (ert-info ("Constructor with contents")
              (setq msg (zmq-message "aa"))
              (should (equal (zmq-message-data msg) "aa"))
              (zmq-close-message msg)
              (ert-info ("Integer vectors")
                (setq msg (zmq-message [10 10]))
                (should (equal (zmq-message-data msg) "\n\n"))
                (zmq-close-message msg)))
            (ert-info ("Constructor with invalid argument")
              (should-error (zmq-message t)
                            :type 'wrong-type-argument))
            (ert-info ("Test `zmq-init-message'")
              (setq contents "msg1"
                    msg (zmq-message))
              (zmq-init-message msg contents)
              (should (= (zmq-message-size msg) (length contents)))
              (should (equal (zmq-message-data msg) contents))
              (should-error (zmq-init-message msg 10)
                            :type 'wrong-type-argument)
              (zmq-init-message msg)
              (should (= (zmq-message-size msg) 0))
              (ert-info ("Integer vectors")
                ;; Vectors of integers are also handled
                (zmq-init-message msg [10 0 10])
                (should (equal (zmq-message-data msg) "\n\0\n"))
                ;; But data is truncated for integers out of the range [0,255]
                ;; since only pure bytes are handled by zmq.
                (zmq-init-message msg [256 0 10])
                (should (equal (zmq-message-data msg) "\0\0\n")))))
        (zmq-close-message msg))))
  (ert-info ("Moving messages")
    (let ((msg1 (zmq-message "msg1"))
          (msg2 (zmq-message "msg2")))
      (unwind-protect
          (progn
            (should (equal (zmq-message-data msg1) "msg1"))
            (should (equal (zmq-message-data msg2) "msg2"))
            (zmq-move-message msg2 msg1)
            (should (equal (zmq-message-data msg2) "msg1"))
            (should (equal (zmq-message-data msg1) "")))
        (zmq-close-message msg1)
        (zmq-close-message msg2))))
  (ert-info ("Copying messages")
    (let ((msg1 (zmq-message "msg1"))
          msg2)
      (unwind-protect
          (progn
            (should (equal (zmq-message-data msg1) "msg1"))
            (unwind-protect
                (progn
                  (setq msg2 (zmq-copy-message msg1))
                  (should (zmq-message-p msg2))
                  (should (equal (zmq-message-data msg2) "msg1"))
                  (should (equal (zmq-message-data msg1) "msg1")))
              (zmq-close-message msg2)))
        (zmq-close-message msg1))))
  (ert-info ("Message properties")
    (let ((msg (zmq-message)))
      (zmq-message-set-id msg 10)
      (should (= (zmq-message-id msg) 10))
      (should-not (zmq-message-get msg zmq-MORE))
      (zmq-close-message msg)))
  (ert-info ("Sending/receiving messages")
    (let ((ctx (zmq-context))
          (msg (zmq-message)))
      (cl-destructuring-bind (p . s)
          (zmq-create-bound-pair ctx zmq-REP zmq-REQ)
        (unwind-protect
            (progn
              (ert-info ("Metadata of received messages")
                (zmq-send s "hello")
                (zmq-recv-message msg p)
                (should (equal (zmq-message-property msg :socket-type) "REQ")))
              (ert-info ("Multipart messages")
                (zmq-init-message msg "foo")
                (zmq-send-message msg p zmq-SNDMORE)
                (zmq-init-message msg "bar")
                (zmq-send-message msg p)
                (zmq-init-message msg)
                (zmq-recv-message msg s)
                (should (equal (zmq-message-data msg) "foo"))
                (should (zmq-message-more-p msg))
                (zmq-init-message msg)
                (zmq-recv-message msg s)
                (should (equal (zmq-message-data msg) "bar"))
                (should-not (zmq-message-more-p msg))))
          (zmq-close-message msg)
          (zmq-close s)
          (zmq-close p)
          (zmq-terminate-context ctx))))))

(ert-deftest zmq-send-unicode ()
  (ert-info ("Unicode messages")
    (let ((msg (zmq-message))
          (contents "[â, â†] = 1"))
      (unwind-protect
          (progn
            ;; Unicode characters are not handled by zmq
            (zmq-init-message msg contents)
            (should (= (zmq-message-size msg) (length contents)))
            (should-not (= (zmq-message-size msg) (string-bytes contents)))
            (should-not (equal (zmq-message-data msg) contents)))
        (zmq-close-message msg)))
    (ert-info ("Sending unicode messages")
      (let* ((addr "tcp://127.0.0.1")
             (ctx (zmq-context)))
        (unwind-protect
            (cl-destructuring-bind (s1 . s2)
                (zmq-create-bound-pair ctx zmq-PAIR zmq-PAIR)
              (unwind-protect
                  (let ((u "çπ§")
                        (s nil))
                    (zmq-send-encoded s1 u)
                    (setq s (zmq-recv s2))
                    (should (equal s (encode-coding-string u 'utf-8)))
                    (should (equal (decode-coding-string s 'utf-8) u))
                    (zmq-send-encoded s1 u 'utf-16)
                    (setq s (zmq-recv-decoded s2 'utf-16))
                    (should (equal s u)))
                (zmq-close s1)
                (zmq-close s2)))
          (zmq-terminate-context ctx))))))

(ert-deftest zmq-sockets ()
  (with-zmq-context
    (let* ((ctx (zmq-current-context))
           (endpoint "tcp://127.0.0.1:5545")
           (s (zmq-socket ctx zmq-PUB)))
      (unwind-protect
          (ert-info ("Connecting sockets")
            (should-error
             (zmq-bind s "photon://a")
             :type 'zmq-EPROTONOSUPPORT)
            (should-error
             (zmq-connect s "photon://a")
             :type 'zmq-EPROTONOSUPPORT)
            (should-error
             (zmq-bind s "tcp://")
             :type 'zmq-EINVAL)
            (zmq-bind s endpoint)
            (should (equal (zmq-socket-get s zmq-LAST_ENDPOINT) endpoint))
            (zmq-unbind s endpoint)
            (zmq-connect s endpoint)
            (should (equal (zmq-socket-get s zmq-LAST_ENDPOINT) endpoint))
            (zmq-disconnect s endpoint))
        (zmq-close s))
      (ert-info ("Socket macro")
        (with-zmq-socket sock zmq-PUB
            ((zmq-LINGER 10))
          (setq s sock)
          (should (= (zmq-socket-get sock zmq-LINGER) 10)))
        (should-error (zmq-close s)
                      :type 'zmq-ENOTSOCK))
      (ert-info ("Socket options")
        (with-zmq-socket sock zmq-PULL
          (ert-info ("Integer options")
            (zmq-set-option sock zmq-CONNECT_TIMEOUT 100)
            (should (= (zmq-get-option sock zmq-CONNECT_TIMEOUT) 100))
            (zmq-set-option sock zmq-AFFINITY 4)
            (should (= (zmq-get-option sock zmq-AFFINITY) 4))
            (zmq-set-option sock zmq-MAXMSGSIZE 256)
            (should (= (zmq-get-option sock zmq-MAXMSGSIZE) 256)))
          (ert-info ("Integer options with boolean types")
            (zmq-set-option sock zmq-CONFLATE t)
            (should (eq (zmq-get-option sock zmq-CONFLATE) t))
            (should-error (zmq-set-option sock zmq-IMMEDIATE 0)
                          :type 'wrong-type-argument))
          (ert-info ("String options")
            (zmq-set-option sock zmq-PLAIN_PASSWORD "password")
            (should (equal (zmq-get-option sock zmq-PLAIN_PASSWORD)
                           "password"))
            ;; No multi-byte
            (should-error (zmq-set-option sock zmq-PLAIN_PASSWORD "paßword")
                          :type 'wrong-type-argument))
          (ert-info ("Binary options")
            (let ((identity "identity\0\0"))
              (zmq-set-option sock zmq-ROUTING_ID identity)
              (should (equal (zmq-get-option sock zmq-ROUTING_ID) identity))))
          (ert-info ("CURVE options")
            (when (zmq-has "curve")
              (cl-destructuring-bind (pub . priv)
                  (zmq-curve-keypair)
                (zmq-set-option sock zmq-CURVE_PUBLICKEY pub)
                (zmq-set-option sock zmq-CURVE_SECRETKEY priv)
                (zmq-set-option sock zmq-CURVE_SERVERKEY priv)
                ;; Always returns the string representation
                (should (equal (zmq-get-option sock zmq-CURVE_PUBLICKEY)
                               pub))
                (should (equal (zmq-get-option sock zmq-CURVE_SECRETKEY)
                               priv))
                (should (equal (zmq-get-option sock zmq-CURVE_SERVERKEY)
                               priv)))))
          (with-zmq-socket sock zmq-SUB
            (ert-info ("Unicode options")
              (let ((topic "tést"))
                (should-error
                 (zmq-set-option sock zmq-SUBSCRIBE topic)
                 :type 'wrong-type-argument)
                (should-error
                 (zmq-set-option sock zmq-ROUTING_ID topic)
                 :type 'wrong-type-argument)
                (zmq-socket-set-encoded sock zmq-ROUTING_ID topic 'utf-16)
                (should (equal (zmq-socket-get-decoded sock zmq-ROUTING_ID 'utf-16)
                               topic))
                (zmq-socket-set-encoded sock zmq-SUBSCRIBE topic 'utf-16)))))))))

(ert-deftest zmq-polling ()
  (let* ((addr "tcp://127.0.0.1"))
    (with-zmq-context
      (ert-info ("`zmq-poll'")
        (cl-destructuring-bind (p . s)
            (zmq-create-bound-pair (zmq-current-context) zmq-PUB zmq-SUB addr)
          (unwind-protect
              (let ((items (list (zmq-pollitem
                                  :socket p :events (list zmq-POLLIN zmq-POLLOUT))
                                 (zmq-pollitem
                                  :socket s :events (list zmq-POLLIN zmq-POLLOUT))))
                    (events nil))

                ;; Allow sockets to connect
                (sleep-for 1.0)

                ;; Subscribe to all incoming messages
                (zmq-socket-set s zmq-SUBSCRIBE "")

                (setq events (zmq-poll items 100))
                (should (member zmq-POLLOUT (alist-get p events)))
                (should-not (alist-get s events))

                (zmq-send p "msg1")
                (setq events (zmq-poll items 100))
                (should (member zmq-POLLOUT (alist-get p events)))

                (sleep-for 0.5)

                (setq events (zmq-poll items 1000))
                (should (member zmq-POLLIN (alist-get s events)))

                (should (equal (zmq-recv s) "msg1"))
                (setq events (zmq-poll items 100))
                (should-not (alist-get s events)))
            (zmq-close s)
            (zmq-close p))))
      (when (zmq-has "draft")
        ;; https://github.com/zeromq/pyzmq/blob/master/examples/poll/pubsub.py
        (ert-info ("`zmq-poller'")
          (cl-destructuring-bind (p . s)
              (zmq-create-bound-pair (zmq-current-context) zmq-PUB zmq-SUB addr)
            (unwind-protect
                (with-zmq-poller poller
                  (let ((events nil))
                    ;; Allow sockets to connect
                    (sleep-for 1.0)

                    ;; Subscribe to all incoming messages
                    (zmq-socket-set s zmq-SUBSCRIBE "")

                    (zmq-poller-register poller p (list zmq-POLLIN zmq-POLLOUT))
                    (zmq-poller-register poller s (list zmq-POLLIN zmq-POLLOUT))

                    (setq events (zmq-poller-wait-all poller 10 100))
                    (should (member zmq-POLLOUT (alist-get p events)))
                    (should-not (alist-get s events))

                    (zmq-send p "msg1")
                    (setq events (zmq-poller-wait-all poller 10 100))
                    (should (member zmq-POLLOUT (alist-get p events)))

                    (sleep-for 0.5)

                    (setq events (zmq-poller-wait-all poller 10 1000))
                    (should (member zmq-POLLIN (alist-get s events)))

                    (should (equal (zmq-recv s) "msg1"))
                    (setq events (zmq-poller-wait-all poller 10 100))
                    (should-not (alist-get s events))))
              (zmq-close s)
              (zmq-close p))))))))

(ert-deftest zmq-subprocess ()
  (ert-info ("Validating sexp")
    (let (proc)
      (unwind-protect
          (progn
            (should-error (setq proc (zmq-start-process (list 1 2 3))))
            ;; TODO: How to check for closures? Tests are normally not byte
            ;; compiled which is when closures are created. This `lexical-let'
            ;; expands into a let form, not a closure.
            (should-error (setq proc (zmq-start-process
                                      (lexical-let ((foo nil))
                                        (lambda () (setq foo 1))))))
            (ert-info ("Only functions with 0 or 1 arguments")
              (should-error (setq proc (zmq-start-process
                                        (lambda (a b)))))))
        (when proc
          (delete-process proc)))))
  (ert-info ("Subprocess wraps function with context")
    (let* ((body (quote ((if zmq-current-context
                             (prin1 (cons 'test-result "context"))
                           (prin1 (cons 'test-result "no context")))
                         (zmq-flush 'stdout))))
           (process))
      (lexical-let ((filter-called nil))
        (setq
         process (zmq-start-process
                  `(lambda () ,@body)
                  (lambda (event)
                    (setq filter-called t)
                    (should (equal (cdr event) "no context")))))
        (with-timeout (0.4 nil)
          (while (not filter-called)
            (sleep-for 0.01)))
        (should filter-called)
        (delete-process process)
        (setq filter-called nil)
        (setq
         process (zmq-start-process
                  `(lambda (ctx) ,@body)
                  (lambda (event)
                    (setq filter-called t)
                    (should (equal (cdr event) "context")))))
        (with-timeout (0.4 nil)
          (while (not filter-called)
            (sleep-for 0.01)))
        (should filter-called)
        (delete-process process))))
  (let ((process (zmq-start-process (lambda () (sleep-for 1000))))
        sexp)
    (unwind-protect
        (ert-info ("Subprocess output")
          (ert-info ("Reading sexp from process output")
            (ert-info ("Reading a single sexp")
              (setq sexp (car (zmq--subprocess-read-output
                               process "(event . \"foo\")")))
              (should (equal sexp '(event . "foo")))
              (should (string= (process-get process :pending-output) ""))
              (should (= (length (buffer-string)) 0)))
            (ert-info ("Reading a partial sexp")
              (setq sexp (car (zmq--subprocess-read-output
                               process "(event . \"foo\"")))
              (should-not sexp)
              (should (equal (process-get process :pending-output) "(event . \"foo\"")))
            (ert-info ("Reading pending output from partial sexp")
              (let ((events (zmq--subprocess-read-output
                             process ")(jelly . jam)")))
                (should (= (length events) 2))
                (should (equal (car events) '(event . "foo")))
                (should (equal (cadr events) '(jelly . jam)))
                (should (string= (process-get process :pending-output) ""))))
            (ert-info ("Invalid read syntax")
              (should-error (zmq--subprocess-read-output
                             process "(foo . bar)#()"))))
          (ert-info ("Subprocess filter")
            (lexical-let ((filter-called nil))
              (process-put
               process :filter (lambda (event)
                                 (setq filter-called t)
                                 (should (equal event '(event . "foo")))))
              (zmq--subprocess-filter process "(event . \"foo\")")
              (should filter-called)
              ;; Subprocess sends errors which get emitted back to the parent
              ;; emacs process, see `zmq--init-subprocess'
              (should-error (zmq--subprocess-filter process "(error . \"foo\")")
                            :type 'zmq-subprocess-error))))
      (kill-process process))))

(provide 'zmq-tests)
