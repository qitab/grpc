;;; Copyright 2021 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

;;;; Public Interface for gRPC

(in-package #:grpc)

(cffi:defcfun ("create_new_grpc_call_details"
               create-grpc-call-details )
  :pointer)

(cffi:defcfun ("delete_grpc_call_details"
               call-details-destroy )
  :void
  (call-details :pointer))

(cffi:defcfun ("start_server" start-server )
  :pointer
  (cq :pointer)
  (server-credentials :pointer)
  (server-address :string))

(cffi:defcfun ("register_method" register-method )
  :pointer
  (server :pointer)
  (method-name :string)
  (server-address :string))

(cffi:defcfun ("grpc_run_server" run-server )
  :pointer
  (server :pointer)
  (server-credentials :pointer))

(cffi:defcfun ("lisp_grpc_server_request_call" grpc-server-request-call )
  :pointer
  (server :pointer)
  (details :pointer)
  (request-metadata :pointer)
  (cq-bound :pointer)
  (cq-notify :pointer)
  (tag :pointer))

(cffi:defcfun ("shutdown_server" shutdown-server )
  :void
  (server :pointer)
  (cq :pointer)
  (tag :pointer))

(defun start-call-on-server (server)
  "Make gRPC SERVER call and return a call struct"
  (let* ((tag (cffi:foreign-alloc :int))
         (metadata (create-new-grpc-metadata-array))
         (call-details (create-grpc-call-details))
         (c-call (grpc-server-request-call server call-details
                                           metadata
                                           grpc::*completion-queue*
                                           grpc::*completion-queue* tag))
         (method (get-call-method call-details)))
    (assert (not (cffi:null-pointer-p c-call)))
    (metadata-destroy metadata)
    (call-details-destroy call-details)
    (cffi:foreign-free tag)
    (grpc::make-call :c-call c-call
                     :c-tag (cffi:null-pointer)
                     :c-ops (cffi:null-pointer)
                     :method-name method
                     :ops-plist nil
                     :is-server-call t)))

(defun send-initial-metadata (call)
  "Send the GRPC_OP_SEND_INITIAL_METADATA from the server through a CALL"
  (declare (type call call))
  (let* ((num-ops 1)
         (c-call (call-c-call call))
         (tag (cffi:foreign-alloc :int))
         (ops (create-new-grpc-ops num-ops))
         (ops-plist (prepare-ops ops :send-metadata t))
         (call-code (call-start-batch c-call ops num-ops tag)))
    (declare (ignore ops-plist))
    (unless (eql call-code :grpc-call-ok)
      (cffi:foreign-free tag)
      (grpc-ops-free ops num-ops)
      (error 'grpc-call-error :call-error call-code))
    (let ((cqp-p (completion-queue-pluck *completion-queue* tag)))
      (grpc-ops-free ops num-ops)
      (cffi:foreign-free tag)
      cqp-p)))

(defun server-send-status (call &optional (status-code :grpc-status-ok) (with-recv-close nil))
  "Send the GRPC_OP_SEND_STATUS_FROM_SERVER from the server through a CALL"
  (declare (type call call))
  (let* ((num-ops (if with-recv-close 2 1))
         (c-call (call-c-call call))
         (tag (cffi:foreign-alloc :int))
         (ops (create-new-grpc-ops num-ops))
         (ops-plist (if with-recv-close
                        (prepare-ops ops :server-recv-close t :server-send-status status-code)
                        (prepare-ops ops :server-send-status status-code)))
         (call-code (call-start-batch c-call ops num-ops tag)))
    (declare (ignore ops-plist))
    (unless (eql call-code :grpc-call-ok)
      (cffi:foreign-free tag)
      (grpc-ops-free ops num-ops)
      (error 'grpc-call-error :call-error call-code))
    (let ((cqp-p (completion-queue-pluck *completion-queue* tag)))
      (grpc-ops-free ops num-ops)
      (cffi:foreign-free tag)
      cqp-p)))

(defun server-recv-close (call)
  "Send the GRPC_OP_RECV_STATUS_ON_CLIENT from the server through a CALL"
  (declare (type call call))
  (let* ((num-ops 1)
         (c-call (call-c-call call))
         (tag (cffi:foreign-alloc :int))
         (ops (create-new-grpc-ops num-ops))
         (ops-plist (prepare-ops ops :server-recv-close t))
         (call-code (call-start-batch c-call ops num-ops tag)))
    (declare (ignore ops-plist))
    (unless (eql call-code :grpc-call-ok)
      (cffi:foreign-free tag)
      (grpc-ops-free ops num-ops)
      (error 'grpc-call-error :call-error call-code))
    (let ((cqp-p (completion-queue-pluck *completion-queue* tag)))
      (grpc-ops-free ops num-ops)
      (cffi:foreign-free tag)
      cqp-p)))

(defun dispatch-requests (methods server &key (exit-count nil))
  "Block on the SERVER for a call then dispatch the call to the
proper method in METHODS based on the call method name. EXIT-COUNT
allows the caller to specify the number of times dispatch-call
can receive a call."
  (loop for calls-received from 0
        while (or (not exit-count)
                  (< calls-received exit-count))
        do
     (let ((call (start-call-on-server server)))
       (unwind-protect
            (let ((method (find (call-method-name call)
                                methods
                                :test #'string=
                                :key #'method-details-name)))
              (send-initial-metadata call)
              (if method
                  (handler-case
                      (let ((response
                             (if (method-details-input-streaming-p method)
                                 (funcall (method-details-action method) call)
                                 (let* ((messages (receive-message call))
                                        (message (apply #'concatenate
                                                        '(array (unsigned-byte 8) (*)) messages))
                                        (deserialized-message
                                         (funcall (method-details-deserializer method) message)))
                                   (funcall (method-details-action method)
                                            deserialized-message call)))))
                        (unless (method-details-output-streaming-p method)
                          (let ((serialized-response
                                 (funcall (method-details-serializer method) response)))
                            (send-message call serialized-response)))
                        (unless (call-server-send-status-p call)
                          (setf (call-server-send-status-p call) t)
                          (server-send-status
                           call :grpc-status-ok (method-details-input-streaming-p method)))
                        (unless (method-details-input-streaming-p method)
                          (server-recv-close call)))
                    (grpc-server-abort (condition)
                      (unless (call-server-send-status-p call)
                        (setf (call-server-send-status-p call) t)
                        (server-send-status call (abort-status-code condition)
                                            (method-details-input-streaming-p method)))
                      (unless (method-details-input-streaming-p method)
                        (server-recv-close call))))
                  (progn
                    (unless (call-server-send-status-p call)
                      (setf (call-server-send-status-p call) t)
                      (server-send-status call :grpc-status-unimplemented nil))
                    (server-recv-close call))))
         (free-call-data call)))))

(defun run-grpc-server (address methods
                        &key
                        (server-creds
                         (grpc-insecure-server-credentials-create))
                        (cq grpc::*completion-queue*)
                        (num-threads 1)
                        (dispatch-requests #'dispatch-requests))
  "Start a gRPC server.
Parameters
  ADDRESS: The address to run the server on.
  METHODS: The methods to start. Should be a list of method-details.
  SERVER-CREDS: Pointer to the gRPC server credentials.
  CQ: The completion queue to use.
  NUM-THREADS: The number of threads to have running.
  DISPATCH-CALL: A function to use to dispatch calls.
                 Useful for debugging."
  (let* ((server (start-server cq server-creds address))
         threads)

    (dolist (method methods)
      (format t "~s~%" (method-details-name method))
      (register-method server (method-details-name method) address))
    (run-server server server-creds)

    (unwind-protect
         (dotimes (i num-threads)
           (push
            (bordeaux-threads:make-thread
             (lambda ()
               (funcall dispatch-requests methods server))
             :name (format nil "Dispatch Request Thread ~a" i))
            threads))

      (dolist (thread threads)
        (bordeaux-threads:join-thread thread))

    (shutdown-server server cq (cffi:foreign-alloc :int)))))
