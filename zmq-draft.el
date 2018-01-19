;;; zmq-draft.el --- Draft API

;; Copyright (C) 2018 Nathaniel Nicandro

;; Author: Nathaniel Nicandro <nathanielnicandro@gmail.com>
;; Created: 19 Jan 2018
;; Version: 0.0.1

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Currently the only draft API supported is the the zmq_poller interface.

;;; Code:

(require 'zmq-constants)
(require 'ffi)
(require 'zmq-ffi)

(define-ffi-struct zmq--poller-event-t
  (socket :type :pointer)
  (fd :type :int)
  (user-data :type :pointer)
  (events :type :short))

(cl-defstruct (zmq-poller
               (:constructor nil)
               (:constructor
                zmq-poller
                (&aux (-ptr (or (zmq--poller-new)
                                (error "Poller not created"))))))
  (-ptr nil :read-only t)
  (-socks-fds nil))

;; TODO: Handle windows machines
(eval-and-compile
  ;; See `zmq-poller' type
  (zmq--ffi-wrapper "poller_new" :pointer ())
  (zmq--ffi-wrapper "poller_destroy" :int ((pollerp :pointer)))
  (zmq--ffi-wrapper "poller_add" :int ((poller :poller) (sock :socket) (user-data :pointer) (events :short)))
  (zmq--ffi-wrapper "poller_add_fd" :int ((poller :poller) (fd :int) (user-data :pointer) (events :short)))
  (zmq--ffi-wrapper "poller_modify" :int ((poller :poller) (sock :socket) (events :short)))
  (zmq--ffi-wrapper "poller_modify_fd" :int ((poller :poller) (fd :int) (events :short)))
  (zmq--ffi-wrapper "poller_remove" :int ((poller :poller) (sock :socket)))
  (zmq--ffi-wrapper "poller_remove_fd" :int ((poller :poller) (fd :int)))
  (zmq--ffi-wrapper "poller_wait" :int ((poller :poller) (event :pointer) (timeout :long)))
  (zmq--ffi-wrapper "poller_wait_all" :int ((poller :poller) (events :pointer) (nevents :int) (timeout :long))))

(defun zmq-destroy-poller (poller)
  "Destroy a POLLER."
  (let ((ptr (zmq-poller--ptr poller)))
    (with-ffi-temporary (pptr :pointer)
      (ffi--mem-set pptr :pointer (ffi-pointer+ ptr 0))
      (zmq--poller-destroy pptr))))

(defun zmq-poller-add (poller sock-or-fd events &optional user-data)
  "Listen for EVENTS on SOCK-OR-FD using POLLER.

SOCK-OR-FD can either be a `zmq-socket' or a file descriptor.
EVENTS can either be a list of events (one of `zmq-POLLIN',
`zmq-POLLOUT', `zmq-POLLERR') or a bitwise-or of events. Optional
arguments USER-DATA is currently ignored."
  (let ((events (if (listp events) (apply #'logior events)
                  events)))
    (when (condition-case nil
              (zmq-modify-poller poller sock-or-fd events)
            (zmq-EINVAL t))
      (setq user-data (or user-data (ffi-null-pointer)))
      (if (integerp sock-or-fd)
          (zmq--poller-add-fd poller sock-or-fd user-data events)
        (zmq--poller-add poller sock-or-fd user-data events))
      (setf (zmq-poller--socks-fds poller)
            (cons sock-or-fd (zmq-poller--socks-fds poller))))))

(defun zmq-modify-poller (poller sock-or-fd events)
  "Modify the EVENTS of SOCK-OR-FD that POLLER listens for."
  (let ((events (if (listp events) (apply #'logior events)
                  events)))
    (if (integerp sock-or-fd)
        (zmq--poller-modify-fd poller sock-or-fd events)
      (zmq--poller-modify poller sock-or-fd events))))

(defun zmq-poller-remove (poller sock-or-fd)
  "Remove SOCK-OR-FD from POLLER."
  (when (condition-case nil
            (prog1 t
              (if (integerp sock-or-fd)
                  (zmq--poller-remove-fd poller sock-or-fd)
                (zmq--poller-remove poller sock-or-fd)))
          (zmq-EINVAL nil))
    (setf (zmq-poller--socks-fds poller)
          (cl-remove sock-or-fd (zmq-poller--socks-fds poller)
                     :test (lambda (a b)
                             (or (and (zmq-socket-p a)
                                      (zmq-socket-p b)
                                      (ffi-pointer= (zmq-socket--ptr a)
                                                    (zmq-socket--ptr b)))
                                 (and (integerp a) (integerp b) (= a b))))))))

(defun zmq-poller-register (poller sock-or-fd events)
  "Register the EVENTS of SOCK-OR-FD on POLLER."
  (zmq-poller-add poller sock-or-fd events))

(defun zmq-poller-unregister (poller sock-or-fd)
  "Unregister SOCK-OR-FD from POLLER."
  (zmq-poller-remove poller sock-or-fd))

(defun zmq--poller-event-trigger (poller event)
  (let ((esock (zmq--poller-event-t-socket event)))
    (or (unless (ffi-pointer-null-p esock)
          (cl-loop
           for s in (zmq-poller--socks-fds poller)
           when (and (zmq-socket-p s) (ffi-pointer= (zmq-socket--ptr s) esock))
           return s))
        (zmq--poller-event-t-fd event))))

(defun zmq-poller-wait (poller timeout)
  "Poll for an event with POLLER until TIMEOUT ms.

If an event occures before TIMEOUT ms, return a cons
cell (SOCK-OR-FD . EVENTS) where EVENTS is a list of events which
occured before TIMEOUT. Otherwise return nil. If TIMEOUT is -1,
wait forever until an event arrives."
  (with-ffi-temporaries ((e zmq--poller-event-t))
    (condition-case nil
        (when (>= (zmq--poller-wait poller e timeout) 0)
          (cons (zmq--poller-event-trigger poller e)
                (zmq--split-poll-events (zmq--poller-event-t-events e))))
      (zmq-ETIMEDOUT nil))))

(defun zmq-poller-wait-all (poller nevents timeout)
  "Wait until TIMEOUT for NEVENTS on POLLER.

If between 1 and NEVENTS events occured within TIMEOUT (measured
in milliseconds) return a list of cons cells, each element having
the form (SOCK-OR-FD . EVENTS). EVENTS is a list of events which
occured on SOCK-OR-FD during the polling period. Note that the
length of the returned list may be less than NEVENTS if less than
NEVENTS events occurred within TIMEOUT. If TIMEOUT is -1, wait
forever."
  (let ((size (ffi--type-size zmq--poller-event-t)))
    (with-ffi-temporaries ((es (* size nevents)))
      (condition-case nil
          (let ((found (zmq--poller-wait-all poller es nevents timeout))
                (events nil)
                (e nil))
            (while (>= (setq found (1- found)) 0)
              ;; TODO: What about user-data?
              (setq
               e (ffi-pointer+ es (* found size))
               events (cons (cons (zmq--poller-event-trigger poller e)
                                  (zmq--split-poll-events
                                   (zmq--poller-event-t-events e)))
                            events)))
            events)
        (zmq-ETIMEDOUT nil)))))

(provide 'zmq-draft)

;;; zmq-draft.el ends here
