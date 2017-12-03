(require 'zmq-ffi)
(require 'cl-lib)

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
                    (search-forward "((" (line-end-position) t))
               4 2)
           col)
      (goto-char here))))

(defun zmq--indent-3 (pos state)
  (zmq--indent 3 pos state))

(defun zmq--indent-4 (pos state)
  (zmq--indent 4 pos state))

(defmacro with-zmq-context (ctx &rest body)
  (declare (indent 1) (debug t))
  `(let ((,ctx (zmq-ctx-new)))
     (unwind-protect
         (progn ,@body)
       (zmq-ctx-term ,ctx))))

(defmacro with-zmq-socket (ctx sock type &optional options &rest body)
  "Run BODY with a new socket, SOCK, that has type, TYPE.

The context, CTX, is used to instantiate the socket. Any socket
OPTIONS will be set before running BODY. After BODY has run, the
LINGER option of the socket is set to 0 before the socket is
finally closed."
  (declare (indent zmq--indent-4))
  (let ((sock-options
         (if (and (listp options)
                  ;; Ensure options is a list of bindings (in the sense of let)
                  (null (cl-find-if-not
                         (lambda (x) (and (listp x) (= (length x) 2)))
                         options)))
             (cl-loop
              for (option value) in options
              collect `(zmq-setsockopt ,sock ,option ,value))
           ;; Otherwise options must be part of body
           (setq body (cons options body))
           nil)))
    `(let ((,sock (zmq-socket ,ctx ,type)))
       (unwind-protect
           (progn
             ,@sock-options
             ,@body)
         ;; http://zguide.zeromq.org/page:all#Making-a-Clean-Exit
         ;;
         ;; NOTE: Alternatively set zmq-BLOCKY on the context before creating a
         ;; socket
         (zmq-setsockopt ,sock zmq-LINGER 0)
         (zmq-close ,sock)))))

(defmacro with-zmq-msg (msg data &rest body)
  (declare (indent 2))
  `(let ((,msg (zmq-msg-new ,data)))
     (unwind-protect
         (progn ,@body)
       (zmq-msg-close ,msg))))

(defun zmq--bind-connect-endpoint (bind sock-type endpoint options fun)
  (let ((conn-fun (if bind 'zmq-bind 'zmq-connect))
        (sock-options (cl-loop
                       for (option value) in options
                       collect `(zmq-setsockopt sock ,option ,value))))
    `(zmq-start-process
      (lambda (ctx)
        (let ((fun ,(if (symbolp fun) (symbol-function fun)
                      fun)))
          (with-zmq-socket ctx sock ,sock-type
            ,@sock-options
            (,conn-fun sock ,endpoint)
            (funcall fun ctx sock)))))))

(defmacro zmq-connect-to-endpoint (sock-type endpoint options fun)
  (declare (indent zmq--indent-3))
  (zmq--bind-connect-endpoint nil sock-type endpoint options fun))

(defmacro zmq-bind-to-endpoint (sock-type endpoint options fun)
  "Start a subprocess with a socket bound to ENDPOINT and run
FUN."
  (declare (indent zmq--indent-3))
  (zmq--bind-connect-endpoint 'bind sock-type endpoint options fun))

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
      (with-zmq-context ctx
        (apply fun ctx args))
    (apply fun args)))

(defun zmq-init-subprocess ()
  (if noninteractive
      (let ((coding-system-for-write 'utf8-unix))
        ;; TODO: Better error handling
        (condition-case err
            (let* ((sexp (read (decode-coding-string
                                ;; read from stdin without prompting
                                (base64-decode-string (read-minibuffer ""))
                                'utf-8-unix))))
              (setq sexp (eval sexp))
              (zmq-subprocess-validate-function sexp)
              (zmq-subprocess-start-function
               sexp (= (length (cadr sexp)) 1)))
          (error (prin1 err) (prin1 "\n"))))
    (error "Only run this in a subprocess.")))

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
         (coding-system-for-read 'utf-8-unix)
         (process (make-process
                   :name "zmq"
                   :buffer (generate-new-buffer " *zmq*")
                   :connection-type 'pipe
                   :sentinel #'zmq-subprocess-sentinel
                   :command (list
                             (file-truename
                              (expand-file-name invocation-name
                                                invocation-directory))
                             "-Q" "-batch"
                             "-L" (file-name-directory (locate-library "ffi"))
                             "-L" (file-name-directory (locate-library "zmq"))
                             "-l" (locate-library "zmq")
                             "-f" "zmq-init-subprocess"))))
    (zmq-subprocess-send process (macroexpand-all sexp))
    process))

(provide 'zmq)
