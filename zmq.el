(require 'zmq-ffi)
(require 'cl-lib)

(defvar zmq-current-context nil
  "Dynamically bound to the current context in `with-zmq-context'.
Accessed this variable through `current-zmq-context'.")
;;
(defun zmq--indent (nspecial pos state)
  (let ((here (point))
        (nargs 0)
        (col nil))
    (goto-char (1+ (nth 1 state)))
    (setq col (1- (current-column)))
    (forward-sexp)
    (catch 'parsed-enough
      (while (< (point) (nth 2 state))
        (forward-sexp)
        (setq nargs (1+ nargs))
        (when (>= nargs nspecial)
          (throw 'parsed-enough t))))
    (goto-char pos)
    (prog1
        ;; Allow the third argument (and preceding arguments) to indent to 4 if
        ;; followed by another argument. Otherwise the third argument indents
        ;; to 2. This is to handle socket options lists which may or may not be
        ;; present.
        (+ (if (and (< nargs nspecial)
                    (re-search-forward "^[ \t]*([ \t]*(" (line-end-position) t))
               4 2)
           col)
      (goto-char here))))

(defun zmq--indent-3 (pos state)
  (zmq--indent 3 pos state))

(defun zmq--indent-4 (pos state)
  (zmq--indent 4 pos state))

(defmacro with-zmq-context (&rest body)
  (declare (indent 0) (debug (symbolp &rest form)))
  ;; use --ctx-- just in case any shenanigans happen with `zmq-current-context'
  ;; while running body.
  `(let* ((--ctx--  (zmq-context))
          (zmq-current-context --ctx--))
     (unwind-protect
         (progn ,@body)
       (while (condition-case err
                  (progn
                    (zmq-terminate-context --ctx--)
                    nil)
                (zmq-EFAULT nil)
                (zmq-EINTR t)
                (error (signal (car err) (cdr err))))))))

(defun current-zmq-context ()
  (when zmq-current-context
    (condition-case nil
        ;; Try to get an option to see if the context is still valid
        (zmq-context-get zmq-current-context zmq-BLOCKY)
      (zmq-EFAULT (setq zmq-current-context nil))))
  (or zmq-current-context
      ;; TODO: Better error
      (signal 'zmq-ERROR '("No context available."))))

(defmacro with-zmq-socket (sock type &optional options &rest body)
  "Run BODY with a new socket, SOCK, that has type, TYPE.

The context, CTX, is used to instantiate the socket. Any socket
OPTIONS will be set before running BODY. After BODY has run, the
LINGER option of the socket is set to 0 before the socket is
finally closed."
  (declare (debug (symbolp form &optional form &rest form))
           (indent zmq--indent-3))
  (let ((sock-options
         (if (and (listp options)
                  ;; Ensure options is a list of bindings (in the sense of let)
                  (null (cl-find-if-not
                         (lambda (x) (and (listp x) (= (length x) 2)))
                         options)))
             (cl-loop
              for (option value) in options
              collect `(zmq-set-option ,sock ,option ,value))
           ;; Otherwise options must be part of body
           (setq body (cons options body))
           nil)))
    `(let* ((--ctx-- (current-zmq-context))
            (,sock (if --ctx-- (zmq-socket --ctx-- ,type)
                     ;; TODO: Better errors.
                     (signal 'zmq-ERROR '("No `current-zmq-context'.")))))
       (unwind-protect
           (progn
             ,@sock-options
             ,@body)
         ;; http://zguide.zeromq.org/page:all#Making-a-Clean-Exit
         ;;
         ;; NOTE: Alternatively set zmq-BLOCKY on the context before creating a
         ;; socket
         (zmq-set-option ,sock zmq-LINGER 0)
         (zmq-close ,sock)))))

;; FIXME: Freeing initialized messages doesn't work due to how closures work.
;; Maybe its time to implement a c-module for zmq?
(defmacro with-zmq-message (message data &rest body)
  (declare (debug t) (indent 2))
  `(let ((,message (zmq-message ,data)))
     (unwind-protect
         (progn ,@body)
       (zmq-close-message ,message))))

(defvar zmq-current-poller nil
  "Dynamically bound in `with-zmq-poller'. Access through
`current-zmq-poller'.")

(defun current-zmq-poller ()
  zmq-current-poller)

(defmacro with-zmq-poller (&rest body)
  `(let* ((--poller-- (zmq-poller))
          (zmq-current-poller --poller--))
     (unwind-protect
         (progn ,@body)
       (zmq-poller-destroy --poller--))))

;;; Message functions

(defun zmq-send-multipart (sock parts &optional flags)
  "Send a multipart message with PARTS on SOCK with FLAGS."
  (let ((part (zmq-message)) data)
    (unwind-protect
        (while (setq data (car parts))
          (zmq-init-message part data)
          (setq parts (cdr parts))
          (zmq-message-set part zmq-MORE (not (null parts)))
          (zmq-send-message part sock flags))
      (zmq-close-message part))))

(defun zmq-recv-multipart (sock &optional flags)
  "Receive a multipart message from SOCK."
  (let ((part (zmq-message)) res)
    (unwind-protect
        (catch 'recvd
          (while t
            (zmq-init-message part)
            (zmq-recv-message part sock flags)
            (setq res (cons (zmq-message-data part) res))
            (unless (zmq-message-more-p part)
              (throw 'recvd (nreverse res)))))
      (zmq-close-message part))))

;; TODO: Use this somewhere else
;; (sock-options (cl-loop
;;                for (option value) in options
;;                collect `(zmq-setsockopt sock ,option ,value)))
(defun zmq--bind-connect-endpoint (bind sock-type endpoint fun)
  (let ((conn-fun (if bind 'zmq-bind 'zmq-connect)))
    `(lambda (ctx)
       (let ((fun ,(if (symbolp fun) (symbol-function fun)
                     fun)))
         (with-zmq-socket sock ,sock-type
           (,conn-fun sock ,endpoint)
           (funcall fun ctx sock))))))

(defun zmq-connect-to-endpoint (sock-type endpoint fun)
  (declare (indent 2))
  (zmq-start-process
   (zmq--bind-connect-endpoint nil sock-type endpoint fun)))

(defun zmq-bind-to-endpoint (sock-type endpoint fun)
  "Start a subprocess with a socket bound to ENDPOINT and run
FUN."
  (declare (indent 2))
  (zmq-start-process
   (zmq--bind-connect-endpoint 'bind sock-type endpoint fun)))

;;; Socket functions

(defun zmq-bind-to-random-port (sock addr &optional min-port max-port max-tries)
  "Bind SOCK to ADDR on a random port.

ADDR must be an address string without the port that will be
passed to `zmq-bind' if a port is found. Optional arguments
MIN-PORT (inclusive) and MAX-PORT (exclusive) give a range that
the port number will have if `zmq-bind' succeeds within
MAX-TRIES. MIN-PORT defaults to 49152, MAX-PORT defaults to
65536, and MAX-TRIES defaults to 100. If `zmq-bind' succeded the
port that was bound is returned, otherwise nil is returned."
  (setq min-port (or min-port 49152)
        max-port (or max-port 65536)
        max-tries (or max-tries 100))
  (let (port)
    (catch 'bound
      (dotimes (i max-tries)
        (setq port (+ (cl-random (- max-port min-port)) min-port))
        (condition-case err
            (progn
              (zmq-bind sock (format "%s:%d" addr port))
              (throw 'bound port))
          ((zmq-EACCES zmq-EADDRINUSE)
           (when (eq (car err) 'zmq-EADDRINUSE)
             (unless (eq system-type 'windows-nt)
               (signal (car err) (cdr err)))))
          (error (signal (car err) (cdr err))))))))

(defun zmq-send-encoded (sock str &optional coding-system)
  (setq coding-system (or coding-system 'utf-8))
  (zmq-send sock (encode-coding-string str coding-system)))

(defun zmq-recv-decoded (sock &optional coding-system)
  (setq coding-system (or coding-system 'utf-8))
  (decode-coding-string (zmq-recv sock) coding-system))

(defun zmq-socket-set-encoded (sock option value &optional coding-system)
  (setq coding-system (or coding-system 'utf-8))
  (zmq-set-option sock option (encode-coding-string value coding-system)))

(defun zmq-socket-get-decoded (sock option &optional coding-system)
  (setq coding-system (or coding-system 'utf-8))
  (decode-coding-string (zmq-get-option sock option) coding-system))

;;; Subprocceses
;; TODO: Use `process-put' and `process-get' to control `zmq' subprocesses.

(defun zmq-subprocess-validate-function (sexp)
  "Called in subprocesses to validate the function passed to it.
If a function is not valid, no work will be performed and the
error will be sent to the subprocess' buffer."
  (unless (functionp sexp)
    (signal 'void-function
            "Can only run functions in subprocess."))
  (unless (member (length (cadr sexp)) '(0 1))
    (signal 'wrong-number-of-arguments
            "Functions can only be passed a context or nothing.")))

(defun zmq-subprocess-start-function (fun &optional wrap-context &rest args)
  (if wrap-context
      (with-zmq-context
        (apply fun (current-zmq-context) args))
    (apply fun args)))

(defun zmq-flush (stream)
  "Flush STREAM.

STREAM can be one of `stdout', `stdin', or `stderr'."
  (set-binary-mode stream t)
  (set-binary-mode stream nil))

(defun zmq-prin1 (sexp)
  "Same as `prin1' but flush `stdout' afterwards."
  (prin1 sexp)
  (zmq-flush 'stdout))

(defun zmq-init-subprocess ()
  (if (not noninteractive) (error "Not a subprocess.")
    (prin1 '(start))
    (condition-case err
        (let ((coding-system-for-write 'utf-8-unix)
              (cmd (read (decode-coding-string
                          (base64-decode-string (read))
                          'utf-8-unix))))
          (cl-case (car cmd)
            (eval (let ((sexp (cdr cmd)))
                    (zmq-prin1 '(eval . "START"))
                    (setq sexp (eval sexp))
                    (zmq-subprocess-validate-function sexp)
                    (zmq-subprocess-start-function
                     sexp (= (length (cadr sexp)) 1))
                    (zmq-prin1 '(eval . "STOP"))))
            (stop (prin1 '(stop))
                  (signal 'quit '(zmq-subprocess)))))
      (error (prin1 (cons 'error err))))))

(defun zmq-subprocess-sentinel (process event)
  (cond
   ;; TODO: Handle other events
   ((or (string= event "finished\n")
        (string-prefix-p "exited" event)
        (string-prefix-p "killed" event))
    (with-current-buffer (process-buffer process)
      (when (or (not (buffer-modified-p))
                (= (point-min) (point-max)))
        (kill-buffer (process-buffer process)))))))

(defun zmq-subprocess-echo-output (process output)
  (when (buffer-live-p (process-buffer process))
    (with-current-buffer (process-buffer process)
      (let ((moving (= (point) (process-mark process))))
        (save-excursion
          (goto-char (process-mark process))
          (insert output)
          (set-marker (process-mark process) (point)))
        (if moving (goto-char (process-mark process)))))))

(defun zmq-subprocess-read-sexp (process)
  ;; TODO: Don't rely on `current-buffer'
  (let ((sexp (condition-case err
                  (read (current-buffer))
                (end-of-file
                 (prog1 nil
                   (let ((pending (buffer-string)))
                     (unless (> (length pending) 0)
                       (process-put
                        process :pending-output pending))))))))
    (when sexp
      (goto-char (point-min))
      (delete-region (point) (save-excursion (forward-sexp 1) (point))))
    sexp))

(defun zmq-subprocess-insert-output (process output)
  (let ((pending (process-get process :pending-output)))
    (when pending
      (insert pending)
      (process-put process :pending-output nil))
    (insert output)))

(defun zmq-subprocess-filter (process output)
  (with-temp-buffer
    (zmq-subprocess-insert-output process output)
    (goto-char (point-min))
    ;; TODO: Messages can cross output boundary
    (let (sexp)
      (while (setq sexp (zmq-subprocess-read-sexp process))
        ;; Only lists are processed, anything that resolves into a symbol is
        ;; text.
        ;; TODO: Collect them and echo to output
        (unless (symbolp sexp)
          (message "Received %s from %s" sexp process)
          (cond
           ((consp sexp)
            (let ((state (car sexp))
                  (data (cdr sexp)))
              (cond
               ((eq state 'io-event)
                (let* ((sock (cl-find-if (lambda (s) (= (zmq-socket-get s zmq-FD)
                                                   (car data)))
                                         (process-get process :io-sockets)))
                       ;; See ZMQ_EVENTS
                       ;; http://api.zeromq.org/4-2:zmq-getsockopt
                       (sock-events (zmq-socket-get sock zmq-EVENTS))
                       ;; Handled by sock-events
                       (event (cdr data))
                       (on-recv (process-get process :on-recv))
                       (on-send (process-get process :on-send)))
                  (when (and on-recv (/= (logand sock-events zmq-POLLIN) 0))
                    (funcall on-recv (zmq-recv-multipart sock)))
                  (when (and on-send (/= (logand sock-events zmq-POLLOUT) 0))
                    ;; TODO: How to capture the sent message?
                    (funcall on-send "<msg>"))))
               ((eq state 'port) (process-put process :port data)))))))))))

;; Adapted from `async--insert-sexp' in the `async' package :)
(defun zmq-subprocess-send (process sexp)
  (declare (indent 1))
  (let ((print-circle t)
        (print-escape-nonascii t)
        print-level print-length)
    (with-temp-buffer
      (prin1 sexp (current-buffer))
      (encode-coding-region (point-min) (point-max) 'utf-8-unix)
      (base64-encode-region (point-min) (point-max) t)
      (goto-char (point-min)) (insert ?\")
      (goto-char (point-max)) (insert ?\" ?\n)
      (process-send-region process (point-min) (point-max)))))

(defun zmq-start-process (sexp)
  (cond
   ((functionp sexp)
    (when (or (not (listp sexp))
              (eq (car sexp) 'function))
      (setq sexp (symbol-function sexp))))
   (t (error "Can only send functions to processes.")))
  (unless (member (length (cadr sexp)) '(0 1))
    (error "Invalid function to send to process, can only have 0 or 1 arguments."))
  (let* ((process-connection-type nil)
         (process (make-process
                   :name "zmq"
                   :buffer (generate-new-buffer " *zmq*")
                   :connection-type 'pipe
                   :sentinel #'zmq-subprocess-sentinel
                   :filter #'zmq-subprocess-filter
                   :coding-system 'no-conversion
                   :command (list
                             (file-truename
                              (expand-file-name invocation-name
                                                invocation-directory))
                             "-Q" "-batch"
                             "-L" (file-name-directory (locate-library "ffi"))
                             "-L" (file-name-directory (locate-library "zmq"))
                             "-l" (locate-library "zmq")
                             "-f" "zmq-init-subprocess"))))
    (zmq-subprocess-send process (cons 'eval (macroexpand-all sexp)))
    process))

;;; Streams

(defun zmq-ioloop (socks on-recv on-send)
  (declare (indent 1))
  (unless (listp socks)
    (setq socks (list socks)))
  (let* ((fds (mapcar (lambda (sock) (zmq-socket-get sock zmq-FD)) socks))
         (process
          (zmq-start-process
           `(lambda ()
              (let* ((items (list ,@(mapcar (lambda (fd)
                                         `(zmq-pollitem
                                           :fd ,fd
                                           :events ,(logior zmq-POLLIN
                                                            zmq-POLLOUT)))
                                       fds))))
                (while t
                  (let ((events (condition-case err
                                    (zmq-poll items 1000000)
                                  ;; TODO: This was the error that
                                  ;; `zmq-poller-wait' returned, is it the same
                                  ;; on all systems? Or is this a different
                                  ;; name since I am on a MAC
                                  (zmq-ETIMEDOUT nil)
                                  (error (signal (car err) (cdr err))))))
                    (when events
                      (while (cdr events)
                        (prin1 (cons 'io-event (car events)))
                        (setq events (cdr events)))
                      (prin1 (cons 'io-event (car events)))
                      (zmq-flush 'stdout))
                    (sleep-for 10)
                    ;; Handle communication between parent process. For
                    ;; example receiving commands to do something else or quit.
                    )))))))
    (process-put process :io-sockets socks)
    (process-put process :on-recv on-recv)
    (process-put process :on-send on-send)
    process))


(provide 'zmq)
