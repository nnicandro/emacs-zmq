(require 'zmq-ffi)

;;; Subprocceses
;; TODO: Use `process-put' and `process-get' to control `zmq' subprocesses.

(defun zmq-subprocess-validate-sexp (sexp)
  "Validate an sexp that will be sent to a subprocess."
  (cond
   ((functionp sexp)
    (when (or (not (listp sexp))
              (eq (car sexp) 'function))
      (setq sexp (symbol-function sexp))))
   (t (error "Can only send functions to processes.")))
  (unless (member (length (cadr sexp)) '(0 1))
    (error "Invalid function to send to process, can only have 0 or 1 arguments.")))

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
        ;; Ignore raw text which gets converted to symbols
        (unless (symbolp sexp)
          (setq accum (cons sexp accum))))
      (process-put process :pending-output (buffer-substring
                                            last-valid (point-max)))
      accum)))

(defun zmq-subprocess-sentinel (process event)
  (cond
   ;; TODO: Handle other events
   ((or (string= event "finished\n")
        (string-prefix-p "exited" event)
        (string-prefix-p "killed" event))
    (with-current-buffer (process-buffer process)
      (when (and (buffer-live-p (current-buffer))
                 (or (not (buffer-modified-p))
                     (= (point-min) (point-max))))
        (kill-buffer))))))

(define-error 'zmq-subprocess-error "Error in subprocess.")

(defun zmq-subprocess-filter (process output)
  (let ((filter (process-get process :filter)))
    (when filter
      (let ((stream (zmq-subprocess-read-output process output)))
        (cl-loop
         for event in stream
         if (and (listp event) (eq (car event) 'error)) do
         ;; TODO: Better way to handle this
         (signal 'zmq-subprocess-error (cdr event))
         and return nil else do (funcall filter event))))))

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
  (setq sexp (macroexpand-all sexp))
  (zmq-subprocess-validate-sexp sexp)
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
    (zmq-subprocess-send process (cons 'eval sexp))
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
        (while (car events)
          ;; Only send the file-descriptor, since the events are read using the
          ;; zmq-EVENTS property of the corresponding socket in the parent
          ;; process.
          (prin1 (cons 'io (caar events)))
          (setq events (cdr events)))
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
                                      (base64-decode-string (read-minibuffer ""))
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

(defclass zmq-subprocess ()
  ((on-start :documentation "Setup function to call before
  running the body of the process.")
   (on-stop :documentation "Clean-up function to call after body of process has run.")
   (func :documentation "Function which does the work of this subprocess.")
   (process
    :type process
    :documentation "The process object.")))

(provide 'zmq-subprocess)
