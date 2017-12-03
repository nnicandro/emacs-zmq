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
       (zmq-ctx-destroy ,ctx))))

(defmacro with-zmq-socket (ctx sock type &rest body)
  (declare (indent 3))
  `(let ((,sock (zmq-socket ,ctx ,type)))
     (unwind-protect
         (progn ,@body)
       ;; http://zguide.zeromq.org/page:all#Making-a-Clean-Exit
       (zmq-setsockopt ,sock zmq-LINGER 1)
       (zmq-close ,sock))))

(defmacro with-zmq-msg (msg data &rest body)
  (declare (indent 2) (debug t))
  (let ((msg-init (cond
                   ((and (integerp data) (> data 0))
                    `(let ((,msg (zmq-msg-new)))
                       (zmq-msg-init-size ,msg ,data)
                       ,msg))
                   ((stringp data) `(zmq-msg-init-data ,data))
                   (t `(let ((,msg (zmq-msg-new)))
                         (zmq-msg-init ,msg)
                         ,msg)))))
    `(let ((,msg ,msg-init))
       (unwind-protect
           (progn ,@body)
         (zmq-msg-close ,msg)))))


;;; Subprocceses

(defmacro zmq-async-process-environment (variables &rest body)
  (declare (indent 1))
  (let ((variable-forms
         (seq-map (lambda (opt) `(setq-default
                             ,opt ,(if (listp (symbol-value opt))
                                       `(quote ,(symbol-value opt))
                                     (symbol-value opt))))
                  (seq-filter #'boundp variables))))
    `(progn
       (push ,(file-name-directory (locate-library "ffi")) load-path)
       (push ,(file-name-directory (locate-library "zmq")) load-path)
       (require 'zmq)
       ,@variable-forms
       ,@body)))

(defun zmq-start-subprocess ()

  )

(zmq-async-process-environment ()
  (while t
    ;; listen for messages from kernel
    ))

;; TODO: Reduce load time in subprocesses by splitting functions to separate
;; files. For example `zmq-core' can have all of the context objects and socket
;; API's
(defmacro zmq-start-socket (sock sock-type endpoint &rest body)
  (declare (indent 3))
  (async-start
   `(lambda ()
      (push ,(file-name-directory (locate-library "ffi")) load-path)
      (push ,(file-name-directory (locate-library "zmq")) load-path)
      (require 'zmq)
      (with-zmq-context ctx
        (let ((,sock (zmq-socket ctx ,sock-type)))
          (zmq-bind ,sock ,endpoint)
          (unwind-protect
              (progn ,@body)
            (zmq-unbind ,sock ,endpoint)))))))

;; zmq-bind-and-listen

(defmacro with-zmq-context (ctx &rest body)
  (declare (indent 1))
  `(let ((,ctx (zmq-ctx-new)))
     (unwind-protect
         (progn ,@body)
       (zmq-ctx-close))))

(defun zmq-start-eventloop ()

  )

(provide 'zmq)
