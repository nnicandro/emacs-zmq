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
        (should (should (string= (zmq-z85-decode (zmq-z85-encode public-key))
                                 public-key)))
        (should (string= public-key (zmq-curve-public secret-key)))))))

(ert-deftest zmq-contexts ()
  :tags '(zmq context)
  (let (ctx)
    ;; TODO: Create mock socket object to connect with
    (ert-info ("Test creating contexts")
      (setq ctx (zmq-ctx-new)))
    (ert-info ("Test setting and getting context properties")
      (zmq-ctx-set ctx zmq-MAX_MSGSZ 100)
      (should (= (zmq-ctx-get ctx zmq-MAX_MSGSZ) 100)))
    (ert-info ("Test context termination")
      (zmq-ctx-term ctx))))

(ert-deftest zmq-sockets ()
  :tags '(zmq sockets)
  (ert-info ("Connections")
    (let* ((addr "tcp://127.0.0.1:5580")
           (ctx (zmq-ctx-new))
           (s1 (zmq-socket ctx zmq-PAIR))
           (s2 (zmq-socket ctx zmq-PAIR)))
      (unwind-protect
          (progn
            (zmq-bind s1 addr)
            (zmq-setsockopt s1 zmq-LINGER 0)
            (zmq-connect s2 addr)
            (zmq-setsockopt s2 zmq-LINGER 0)
            (sleep-for 1)
            (should (= (zmq-send s1 "ABC") 3))
            (should (string= (zmq-recv s2 5) "ABC"))
            (ert-info ("Message sending")
              ;; TODO: Test message free
              (let ((msg (zmq-msg-init-data "ABD"))
                    (rmsg (zmq-msg-new)))
                (unwind-protect
                    (progn
                      (should (= (zmq-msg-send msg s1 0) 3))
                      (zmq-msg-init rmsg)
                      (zmq-msg-recv rmsg s2 0)
                      (should (string= (zmq-msg-data rmsg) "ABD")))
                  (zmq-msg-close msg)
                  (zmq-msg-close rmsg)))))
        (zmq-close s1)
        (zmq-close s2)
        (zmq-ctx-term ctx)))))

;; https://github.com/zeromq/pyzmq/blob/master/examples/poll/pair.py
(ert-deftest zmq-polling ()
  :tags '(zmq polling)
  (let* ((addr "tcp://127.0.0.1:5556")
         (ctx (zmq-ctx-new))
         (s1 (zmq-socket ctx zmq-PAIR))
         (s2 (zmq-socket ctx zmq-PAIR)))
    (unwind-protect
        (progn
          (zmq-bind s1 addr)
          (zmq-connect s2 addr)
          (sleep-for 2)
          (let ((items (mapcar (lambda (s) (zmq-pollitem-new
                                  :socket s
                                  :events (logior zmq-POLLIN zmq-POLLOUT)))
                          (list s1 s2))))
            (zmq-poll items 100)
            (should (= (zmq-pollitem-revents (nth 0 items)) zmq-POLLOUT))
            (should (= (zmq-pollitem-revents (nth 1 items)) zmq-POLLOUT))))
      (while (not (condition-case nil
                      (progn
                        (zmq-close s1)
                        (zmq-close s2)
                        (zmq-ctx-term ctx)
                        t)
                    (error (sleep-for 1) nil)))))))

(provide 'zmq-tests)
