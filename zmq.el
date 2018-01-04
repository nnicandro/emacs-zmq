(require 'zmq-ffi)
(require 'zmq-subprocess)
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

(defvar zmq-current-context nil
  "The first `zmq-context' created in this emacs session.")

(defmacro with-zmq-context (&rest body)
  "Wrap BODY with a new `zmq-context' that is terminated when BODY completes.
This is mainly meant to be used in subprocesses. If not in a
subprocess use `current-zmq-context'."
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

(defmacro with-zmq-socket (sock type &optional options &rest body)
  "Run BODY with a new socket, SOCK, with type, TYPE.
If OPTIONS is non-nil it is a list of socket options (in the same
form as `let') which will be set on SOCK before running BODY.

Note that the `current-zmq-context' is used to instantiate SOCK."
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
    `(let ((,sock (zmq-socket (current-zmq-context) ,type)))
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

;;; Setting/Getting options from contexts, sockets, messages

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

(provide 'zmq)
