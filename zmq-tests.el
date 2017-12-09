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
  :tags '(zmq utility)
  (ert-info ("`zmq-version'")
    (let ((version (zmq-version)))
      (should (version<= "4.0.0" version))))
  (ert-info ("`zmq-has'")
    (or (eq (zmq-has "draft") t)
        (eq (zmq-has "draft") nil)))
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
  :tags '(zmq encryption)
  (ert-info ("CURVE mechanism")
    (when (zmq-has "curve")
      (cl-destructuring-bind (public-key . secret-key) (zmq-curve-keypair)
        (should (string= (zmq-z85-decode (zmq-z85-encode public-key))
                         public-key))
        (should (string= public-key (zmq-curve-public secret-key)))))))

(ert-deftest zmq-contexts ()
  :tags '(zmq context)
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
          (setq ctx (current-zmq-context))
          (should (zmq-context-p ctx)))
        (should-error (zmq-terminate-context ctx)
                      :type 'zmq-EFAULT)))))

(ert-deftest zmq-messages ()
  :tags '(zmq message)
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
          (zmq-terminate-context ctx)))))
  (ert-info ("Message macro")
    (let (m)
      (with-zmq-message msg "foo"
        (setq m msg)
        (should (equal (zmq-message-data msg) "foo"))))))

(ert-deftest zmq-send-unicode ()
  :tags '(zmq unicode)
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
  :tags '(zmq sockets)
  (with-zmq-context
    (let* ((ctx (current-zmq-context))
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

;; (setq proc (zmq-start-process
;;             (lambda (ctx)
;;               (zmq-ctx-set ctx zmq-BLOCKY 0)
;;               ;; `with-zmq-socket' uses the `current-zmq-context' to
;;               ;; instantiate a socket
;;               (with-zmq-socket sock zmq-REP
;;                 (let ((port (zmq-bind-to-random-port sock "tcp://127.0.0.1")))
;;                   (when port
;;                     (zmq-prin1 (cons :port port))
;;                     (zmq-recv sock)
;;                     (zmq-send sock "")
;;                     (zmq-recv sock)))))))

;; (zmq-connect-to-endpoint
;;     zmq-REQ (format "tcp://127.0.0.1:%d" (process-get proc :port))
;;   (lambda (ctx sock)
;;     (zmq-prin1 (cons :ctx (zmq-ctx-get ctx zmq-BLOCKY)))
;;     (zmq-send sock "foo")
;;     (zmq-recv sock)
;;     (zmq-send sock "bar")))

(ert-deftest zmq-polling ()
  :tags '(zmq polling)
  (let* ((addr "tcp://127.0.0.1"))
    (with-zmq-context
      (ert-info ("`zmq-poll'")
        (cl-destructuring-bind (p . s)
            (zmq-create-bound-pair (current-zmq-context) zmq-PUB zmq-SUB addr)
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
                (should (member zmq-POLLOUT (cdr (assoc p events))))
                (should-not (assoc s events))

                (zmq-send p "msg1")
                (setq events (zmq-poll items 100))
                (should (member zmq-POLLOUT (cdr (assoc p events))))

                (sleep-for 0.5)

                (setq events (zmq-poll items 1000))
                (should (member zmq-POLLIN (cdr (assoc s events))))

                (should (equal (zmq-recv s) "msg1"))
                (setq events (zmq-poll items 100))
                (should-not (cdr (cl-assoc s events))))
            (zmq-close s)
            (zmq-close p))))
      (when (zmq-has "draft")
        ;; https://github.com/zeromq/pyzmq/blob/master/examples/poll/pubsub.py
        (ert-info ("`zmq-poller'")
          (cl-destructuring-bind (p . s)
              (zmq-create-bound-pair (current-zmq-context) zmq-PUB zmq-SUB addr)
            (unwind-protect
                (with-zmq-poller
                 (let ((poller (current-zmq-poller))
                       (events nil))

                   ;; Allow sockets to connect
                   (sleep-for 1.0)

                   ;; Subscribe to all incoming messages
                   (zmq-socket-set s zmq-SUBSCRIBE "")

                   (zmq-poller-register poller p (list zmq-POLLIN zmq-POLLOUT))
                   (zmq-poller-register poller s (list zmq-POLLIN zmq-POLLOUT))

                   (setq events (zmq-poller-wait-all poller 10 100))
                   (should (member zmq-POLLOUT (cdr (assoc p events))))
                   (should-not (assoc s events))

                   (zmq-send p "msg1")
                   (setq events (zmq-poller-wait-all poller 10 100))
                   (should (member zmq-POLLOUT (cdr (assoc p events))))

                   (sleep-for 0.5)

                   (setq events (zmq-poller-wait-all poller 10 1000))
                   (should (member zmq-POLLIN (cdr (assoc s events))))

                   (should (equal (zmq-recv s) "msg1"))
                   (setq events (zmq-poller-wait-all poller 10 100))
                   (should-not (cdr (cl-assoc s events)))))
              (zmq-close s)
              (zmq-close p))))))))

(ert-deftest zmq-subprocess-filter ()
  :tags '(zmq subprocess)
  (let ((process (start-process "zmq-subprocess-test" nil "sleep" "1000"))
        sexp)
    (unwind-protect
        (ert-info ("Reading sexp from process output")
          (ert-info ("Reading a single sexp")
            (with-temp-buffer
              (zmq-subprocess-insert-output process "(event . \"foo\")")
              (setq sexp (zmq-subprocess-read-sexp process))
              (should (equal sexp '(event . "foo")))
              (should-not (process-get process :pending-output))
              (should (= (length (buffer-string)) 0))))
          (ert-info ("Reading a partial sexp")
            (with-temp-buffer
              (zmq-subprocess-insert-output process "(event . \"foo\"")
              (setq sexp (zmq-subprocess-read-sexp process))
              (should-not sexp)
              (should (equal (process-get process :pending-output) "(event . \"foo\""))))
          (ert-info ("Reading pending output from partial sexp")
            (with-temp-buffer
              (zmq-subprocess-insert-output process ")(jelly . jam)")
              (setq sexp (zmq-subprocess-read-sexp process))
              (should (equal sexp '(event . "foo")))
              (should (equal (buffer-string) "(jelly . jam)"))
              (should-not (process-get process :pending-output)))))
      (kill-process process))))

(defvar zmq-subprocess-test-flag nil)

(ert-deftest zmq-subprocess ()
  :tags '(zmq subprocess)
  (ert-info ("Subprocess wraps function with context")
    (let* ((body
            (quote ((when
                        (condition-case nil
                            (progn (current-zmq-context) t)
                          (error (prin1 (cons 'test-result "no context")) nil))
                      (prin1 (cons 'test-result "context")))
                    (zmq-flush 'stdout))))
           (test-filter
            (lambda (process output)
              (let (sexp)
                (with-temp-buffer
                  (zmq-subprocess-insert-output process output)
                  (catch 'done
                    (while (setq sexp (zmq-subprocess-read-sexp process))
                      (when (and (consp sexp)
                                 (eq (car sexp) 'test-result))
                        (if zmq-subprocess-test-flag
                            (should (equal (cdr sexp) "no context"))
                          (should (equal (cdr sexp) "context")))
                        (throw 'done t))))))
              (when (process-live-p process)
                (kill-process process))))
           (process))
      (setq
       zmq-subprocess-test-flag t
       process (zmq-start-process `(lambda () ,@body)))
      (set-process-filter process test-filter)
      (sleep-for 0.4)
      (setq
       zmq-subprocess-test-flag nil
       process (zmq-start-process `(lambda (ctx) ,@body)))
      (set-process-filter process test-filter)
      (sleep-for 0.4))))

(provide 'zmq-tests)
