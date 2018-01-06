(require 'zmq-ffi)
(require 'cl-lib)

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

;;; Convencience macros for contexts, sockets, and pollers

(defvar zmq-current-context nil
  "The context set by `zmq-current-context'.")

(defmacro with-zmq-context (&rest body)
  "Wrap BODY with a new `zmq-context' that is terminated when BODY completes.
This is mainly meant to be used in subprocesses. If not in a
subprocess use `zmq-current-context'."
  (declare (indent 0) (debug (symbolp &rest form)))
  ;; use --ctx-- just in case any shenanigans happen with `zmq-current-context'
  ;; while running body.
  `(let* ((--ctx--  (zmq-context))
          (zmq-current-context --ctx--))
     (unwind-protect
         (progn ,@body)
       (zmq-terminate-context --ctx--))))

(defmacro with-zmq-socket (sock type &optional options &rest body)
  "Run BODY with a new socket, SOCK, with type, TYPE.
If OPTIONS is non-nil it is a list of socket options (in the same
form as `let') which will be set on SOCK before running BODY.

Note that the `zmq-current-context' is used to instantiate SOCK."
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
    `(let ((,sock (zmq-socket (zmq-current-context) ,type)))
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

(defmacro with-zmq-poller (poller &rest body)
  "Create a new `zmq-poller' bound to POLLER and run BODY.
After BODY is complete call `zmq-poller-destroy' on POLLER."
  (declare (indent 1))
  `(let ((,poller
          (if (zmq-has "draft") (zmq-poller)
            (error "zmq not built with draft API."))))
     (unwind-protect
         (progn ,@body)
       (zmq-poller-destroy ,poller))))

(defun zmq-current-context ()
  "Return the current `zmq-context'.
Return the value of `zmq-current-context'. When there is no
current `zmq-context' (`zmq-current-context' is nil) create a new
one, set it as the `zmq-current-context', and return the newly
created context."
  (when zmq-current-context
    (condition-case nil
        ;; Try to get an option to see if the context is still valid
        (zmq-context-get zmq-current-context zmq-BLOCKY)
      (zmq-EFAULT (setq zmq-current-context nil))))
  (or zmq-current-context
      (setq zmq-current-context (zmq-context))))

;; TODO: Have a global list of open sockets so that we can close any remaining
;; sockets that users have forgotten to close themselves.
(defun zmq-cleanup-on-exit ()
  "Terminate the `zmq-current-context'."
  (when zmq-current-context
    (zmq-terminate-context zmq-current-context)))

(add-hook 'kill-emacs-hook #'zmq-cleanup-on-exit)

;;; Socket functions

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

(defun zmq-bind-to-random-port (sock addr &optional min-port max-port max-tries)
  "Bind SOCK to ADDR on a random port.

ADDR must be an address string without the port that will be
passed to `zmq-bind' if a port is found. Optional arguments
MIN-PORT (inclusive) and MAX-PORT (exclusive) give a range that
the port number will have if `zmq-bind' succeeds within
MAX-TRIES. MIN-PORT defaults to 49152, MAX-PORT defaults to
65536, and MAX-TRIES defaults to 100. If `zmq-bind' succeeds, the
port that was bound is returned. Otherwise nil is returned."
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

;;; Encoding/decoding messages and socket options

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

;;; Sending/receiving multipart messages

(defun zmq-send-multipart (sock parts &optional flags)
  "Send a multipart message with PARTS on SOCK with FLAGS."
  (setq flags (or flags 0))
  (let ((part (zmq-message))
        (data (car parts)))
    (unwind-protect
        (while data
          (zmq-init-message part data)
          (zmq-send-message part sock (if (not (null (cdr parts)))
                                          (logior flags zmq-SNDMORE)
                                        flags))
          (zmq-socket-get sock zmq-EVENTS)
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
            (zmq-socket-get sock zmq-EVENTS)
            (setq res (cons (zmq-message-data part) res))
            (unless (zmq-message-more-p part)
              (throw 'recvd (nreverse res)))))
      (zmq-close-message part))))

;;; Setting/getting options from contexts, sockets, messages

(defun zmq--set-get-option (set object option &optional value)
  (let ((fun (cond
              ((zmq-socket-p object)
               (if set #'zmq-socket-set #'zmq-socket-get))
              ((zmq-context-p object)
               (if set #'zmq-context-set #'zmq-context-get))
              ((zmq-message-p object)
               (if set #'zmq-message-set #'zmq-message-get))
              (t (signal 'wrong-type-argument
                         (list
                          '(zmq-socket-p zmq-context-p zmq-message-p)
                          object))))))
    (if set (funcall fun object option value)
      (funcall fun object option))))

(defun zmq-set-option (object option value)
  "Set an OPTION of OBJECT to VALUE.

OBJECT can be a `zmq-socket', `zmq-context', or a `zmq-message'."
  (zmq--set-get-option 'set object option value))

(defun zmq-get-option (object option)
  "Get an OPTION of OBJECT.

OBJECT can be a `zmq-socket', `zmq-context', or a `zmq-message'."
  (zmq--set-get-option nil object option))

;;; Subprocesses

(define-error 'zmq-subprocess-error "Error in ZMQ subprocess")

(defun zmq-flush (stream)
  "Flush STREAM.
STREAM can be one of `stdout', `stdin', or `stderr'."
  (set-binary-mode stream t)
  (set-binary-mode stream nil))

(defun zmq-prin1 (sexp)
  "Same as `prin1' but flush `stdout' afterwards."
  (prin1 sexp)
  (zmq-flush 'stdout))

(defun zmq--init-subprocess ()
  (if (not noninteractive) (error "Not a subprocess.")
    (let* ((debug-on-event nil)
           (debug-on-error nil)
           (coding-system-for-write 'utf-8-unix)
           (sexp (eval (zmq-subprocess-read)))
           (wrap-context (= (length (cadr sexp)) 1)))
      (setq sexp (byte-compile sexp))
      (if wrap-context
          (with-zmq-context
            (funcall sexp (zmq-current-context)))
        (funcall sexp)))))

(defun zmq--subprocess-read-output (process output)
  "Return a list of s-expressions read from PROCESS' OUTPUT."
  (with-temp-buffer
    (let ((pending (process-get process :pending-output))
          last-valid sexp accum)
      (when (> (length pending) 0)
        (insert pending)
        (process-put process :pending-output ""))
      (insert output)
      (goto-char (point-min))
      (while (setq sexp (condition-case nil
                            (read (current-buffer))
                          (end-of-file
                           (progn (setq accum (nreverse accum))
                                  nil))
                          (invalid-read-syntax
                           (signal 'error (format
                                           "Invalid content: %s"
                                           (buffer-substring (point)
                                                             (point-max)))))))
        (setq last-valid (point))
        ;; FIXME: Ignore raw text which gets converted to symbols
        (unless (symbolp sexp)
          (setq accum (cons sexp accum))))
      (process-put
       process :pending-output (buffer-substring
                                (or last-valid (point-min))
                                (point-max)))
      accum)))

(defun zmq--subprocess-filter (process output)
  (let ((filter (process-get process :filter)))
    (when filter
      (let ((stream (zmq--subprocess-read-output process output)))
        (cl-loop
         for event in stream
         if (and (listp event) (eq (car event) 'error)) do
         ;; TODO: Better way to handle this
         (signal 'zmq-subprocess-error (cdr event))
         else do (funcall filter event))))))

;; Adapted from `async--insert-sexp' in the `async' package :)
(defun zmq-subprocess-send (process sexp)
  "Send SEXP to PROCESS.
This function is meant to be used in combination with
`zmq-subprocess-read'. PROCESS should be an emacs subprocess and
will decode this SEXP using `zmq-subprocess-read' when reading
from STDIN."
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

(defun zmq-subprocess-read ()
  "Read a single s-expression from STDIN.
Note this is only meant to be called from an emacs subprocess."
  (if (not noninteractive) (error "Not in a subprocess.")
    (read (decode-coding-string
           (base64-decode-string (read-minibuffer ""))
           'utf-8-unix))))

(defun zmq-start-process (sexp &optional event-filter sentinel)
  ;; TODO: Mention that closures are not supported
  ;; Validate the sexp, it should be a function which takes 0 or 1 args.
  (cond
   ((functionp sexp)
    (unless (listp sexp)
      (setq sexp (symbol-function sexp))))
   (t (error "Can only send functions to processes.")))
  (unless (member (length (cadr sexp)) '(0 1))
    (error "Invalid function to send to process, can only have 0 or 1 arguments."))
  ;; Create the subprocess
  (let* ((process-connection-type nil)
         (process (make-process
                   :name "zmq"
                   :buffer nil
                   :connection-type 'pipe
                   :coding-system 'no-conversion
                   :filter #'zmq--subprocess-filter
                   :sentinel sentinel
                   :command (list
                             (file-truename
                              (expand-file-name invocation-name
                                                invocation-directory))
                             "-Q" "-batch"
                             "-L" (file-name-directory (locate-library "ffi"))
                             "-L" (file-name-directory (locate-library "zmq"))
                             "-l" (locate-library "zmq")
                             "-f" "zmq--init-subprocess"))))
    (process-put process :filter event-filter)
    (zmq-subprocess-send process (macroexpand-all sexp))
    process))

(provide 'zmq)
