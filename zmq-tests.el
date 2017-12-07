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

(ert-deftest zmq-encryption ()
  :tags '(zmq encryption)
  (ert-info ("Test key encryption")
    (when (zmq-has "curve")
      (cl-destructuring-bind (public-key . secret-key) (zmq-curve-keypair)
        (should (string= (zmq-z85-decode (zmq-z85-encode public-key))
                         public-key))
        (should (string= public-key (zmq-curve-public secret-key)))))))

(ert-deftest zmq-contexts ()
  :tags '(zmq context)
  (let (ctx)
    ;; TODO: Create mock socket object to connect with
    (ert-info ("Test creating contexts")
      (setq ctx (zmq-context)))
    (ert-info ("Test setting and getting context properties")
      (zmq-set-option ctx zmq-MAX_MSGSZ 100)
      (should (= (zmq-get-option ctx zmq-MAX_MSGSZ) 100)))
    (ert-info ("Test context termination")
      (zmq-terminate-context ctx))))


(ert-deftest zmq-sockets ()
  :tags '(zmq sockets)
  (with-zmq-context
    (let ((ctx (current-zmq-context)))
      (ert-info ("Creating sockets")
        (let ((s (zmq-socket ctx zmq-PUB)))
          (should-error
           (zmq-bind s "photon://a")
           :type 'zmq-EPROTONOSUPPORT)
          (should-error
           (zmq-connect s "photon://a")
           :type 'zmq-EPROTONOSUPPORT)
          (should-error
           (zmq-bind s "tcp://")
           :type 'zmq-EINVAL)
          (zmq-close s)))
      (ert-info ("Convenience functions")
        (ert-info ("zmq-bind-to-random-port")
          (with-zmq-socket sock zmq-PUB
            (should-error
             (zmq-bind-to-random-port sock "tcp:*")
             :type 'zmq-EINVAL)
            (should-error
             (zmq-bind-to-random-port sock "rand://")
             :type 'zmq-EPROTONOSUPPORT))))
      (ert-info ("Socket options")
        (ert-info ("Routing ID")
          (with-zmq-socket sock zmq-PULL
            (let ((identity "identity\0\0"))
              (zmq-set-option sock zmq-ROUTING_ID identity)
              (should (equal (zmq-get-option sock zmq-ROUTING_ID)
                             identity)))))
        (ert-info ("Unicode options")
          (cl-destructuring-bind (p . s)
              (zmq-create-bound-pair ctx zmq-PUB zmq-SUB)
            (unwind-protect
                (let ((topic "tést"))
                  (should-error
                   (zmq-set-option s zmq-SUBSCRIBE topic)
                   :type 'wrong-type-argument)
                  (should-error
                   (zmq-set-option s zmq-ROUTING_ID topic)
                   :type 'wrong-type-argument)
                  (zmq-socket-set-encoded s zmq-ROUTING_ID topic 'utf-16)
                  (should-error
                   (zmq-set-option s zmq-AFFINITY topic)
                   :type 'wrong-type-argument)
                  (should-error
                   (zmq-socket-get-decoded s zmq-SUBSCRIBE)
                   :type 'zmq-EINVAL)
                  (should (equal (zmq-socket-get-decoded s zmq-ROUTING_ID 'utf-16)
                                 topic)))
              (zmq-close p)
              (zmq-close s))))))))
(ert-deftest zmq-polling ()
  :tags '(zmq polling)
  (let* ((addr "tcp://127.0.0.1"))
    (with-zmq-context
      ;; https://github.com/zeromq/pyzmq/blob/master/examples/poll/pubsub.py
      (ert-info ("Polling on PUB/SUB sockets")
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
                 (should (equal (cdr (assoc p events)) zmq-POLLOUT))
                 (should-not (assoc s events))

                 (zmq-send p "msg1")
                 (setq events (zmq-poller-wait-all poller 10 100))
                 (should (equal (cdr (assoc p events)) zmq-POLLOUT))

                 (sleep-for 0.5)

                 (setq events (zmq-poller-wait-all poller 10 1000))
                 (should (equal (cdr (assoc s events)) zmq-POLLIN))

                 (zmq-recv s)
                 (setq events (zmq-poller-wait-all poller 10 100))
                 (should-not (cdr (cl-assoc s events)))))
            (zmq-close s)
            (zmq-close p)))))))

(provide 'zmq-tests)
