;; TODO: Fix require's. `with-zmq-context' is required but `zmq-suprocess' is
;; required in `zmq'
(require 'zmq-ffi)

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
            (funcall sexp (current-zmq-context)))
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

(provide 'zmq-subprocess)
