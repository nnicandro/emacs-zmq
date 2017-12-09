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

(when (zmq-has "draft")
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
         (zmq-poller-destroy --poller--)))))

;;; Message functions

(defun zmq-send-multipart (sock parts &optional flags)
  "Send a multipart message with PARTS on SOCK with FLAGS."
  (let ((part (zmq-message))
        (data (car parts)))
    (unwind-protect
        (while data
          (zmq-init-message part data)
          (zmq-message-set part zmq-MORE (not (null (cdr parts))))
          (zmq-send-message part sock flags)
          (setq parts (cdr parts)
                data (car parts)))
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
    (condition-case err
        (let ((coding-system-for-write 'utf-8-unix)
              (cmd (read (decode-coding-string
                          (base64-decode-string
                           (read-minibuffer ""))
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

(defun zmq-subprocess-read (process output)
  "Return a list of cons cells obtained from PROCESS' output.
If the output has any text interlaced with cons cells, the text
is ignored. This may happen for example, when calling `read'. The
right way to read from the parent process from a zmq subprocess
would be to call (read-minibuffer \"\")."
  (with-temp-buffer
    (let ((pending (process-get process :pending-output))
          (last-valid (point))
          (sexp nil)
          (accum nil))
      (when (> (length pending) 0)
        (goto-char (point-min))
        (insert pending)
        (process-put process :pending-output ""))
      (insert output)
      (goto-char last-valid)
      (while (setq sexp (condition-case err
                            (read (current-buffer))
                          (end-of-file (setq accum (nreverse accum)))))
        (setq last-valid (point))
        ;; Ignore printed text that may appear.
        (unless (symbolp sexp)
          (setq accum (cons sexp accum))))
      (process-put process :pending-output (buffer-substring
                                            last-valid (point-max)))
      accum)))

(defun zmq-subprocess-run-callbacks (process sock)
  ;; Note the events are read from the corresponding socket of FD using the
  ;; zmq-EVENTS option of fd's socket. See ZMQ_EVENTS:
  ;; http://api.zeromq.org/4-2:zmq-getsockopt
  (let ((sock-events (zmq-socket-get sock zmq-EVENTS))
        (on-recv (process-get process :on-recv))
        (on-send (process-get process :on-send)))
    (when (and on-recv (/= (logand zmq-POLLIN sock-events) 0))
      (funcall on-recv (zmq-recv-multipart sock)))
    (when (and on-send (/= (logand zmq-POLLOUT sock-events) 0))
      (when (same-class-p sock 'zmq-stream)
        ;; TODO: How to capture the sent message?
        ;; Have a proxy that between the sent events.
        (funcall on-send (zmq-stream-send-queue sock 0))))))

(defun zmq-subprocess-filter (process output)
  (cl-loop
   for (event . contents) in (zmq-subprocess-read process output) do
   (cl-case event
     (eval
      (cond
       ((equal contents "START") (process-put process :eval t))
       ((equal contents "STOP") (process-put process :eval nil))))
     (io
      (let* ((fd contents)
             (sock (cl-find-if (lambda (s) (= (zmq-socket-get s zmq-FD) fd))
                               (process-get process :io-sockets))))
        (zmq-subprocess-run-callbacks process sock)))
     (port (process-put process :port contents)))))

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

(defun zmq-subprocess-poll (items timeout)
  (if (not noninteractive) (error "Not in a subprocess.")
    (let ((events (condition-case err
                      (zmq-poll items timeout)
                    ;; TODO: This was the error that
                    ;; `zmq-poller-wait' returned, is it the same
                    ;; on all systems? Or is this a different
                    ;; name since I am on a MAC
                    (zmq-ETIMEDOUT nil)
                    (error (signal (car err) (cdr err))))))
      (when events
        (while (cdr events)
          ;; Only send the file-descriptor, since the events are read using the
          ;; zmq-EVENTS property of the corresponding socket in the parent
          ;; process.
          (prin1 (cons 'io (caar events)))
          (setq events (cdr events)))
        (prin1 (cons 'io (car events)))
        (zmq-flush 'stdout)))))

(defun zmq-ioloop (socks on-recv on-send)
  (declare (indent 1))
  (unless (listp socks)
    (setq socks (list socks)))
  (let* ((items (mapcar (lambda (fd)
                     (zmq-pollitem
                      :fd fd
                      :events (logior zmq-POLLIN zmq-POLLOUT)))
                   (mapcar (lambda (sock) (zmq-socket-get sock zmq-FD))
                      socks)))
         (process
          (zmq-start-process
           `(lambda ()
              ;; Note that we can splice in `zmq-pollitem's here because they
              ;; only contain primitive types, lists, and vectors.
              (let* ((items ',items))
                (while t
                  ;; Poll for 100 μs
                  (zmq-subprocess-poll items 100)
                  (when (input-pending-p)
                    ;; TODO: Partial messages?
                    (let ((cmd (read (decode-coding-string
                                      (base64-decode-string (read))
                                      'utf-8-unix))))
                      (cl-case (car cmd)
                        (modify-events (setq items (cdr cmd))))))))))))
    (process-put process :io-sockets socks)
    (process-put process :on-recv on-recv)
    (process-put process :on-send on-send)
    process))

(defun zmq-ioloop-modify-events (process items)
  (let ((socks (process-get process :io-sockets)))
    (if (null socks)
        (error "Cannot modify non-ioloop process.")
      (let ((non-item
             (cl-find-if-not
              (lambda (x)
                (and (zmq-pollitem-p x)
                     ;; Only modify events of sockets that PROCESS is polling.
                     (let ((xsock (zmq-pollitem-socket x)))
                       (if xsock
                           (cl-member xsock socks :test #'zmq-socket-equal)
                         (cl-member
                          (zmq-pollitem-fd x) socks
                          :test (lambda (xfd sock)
                                  (= xfd (zmq-socket-get sock zmq-FD))))))))
              items)))
        (when non-item
          (signal 'args-out-of-range
                  (list "Attempting to modify socket not polled by subprocess.")))
        (zmq-subprocess-send process (cons 'modify-events items))))))

(provide 'zmq)
