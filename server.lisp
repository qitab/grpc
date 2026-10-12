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

(cffi:defcfun ("register_server_completion_queue"
               register-server-completion-queue )
  :void
  (server :pointer)
  (cq :pointer))

(cffi:defcfun ("shutdown_server" shutdown-server )
  :void
  (server :pointer)
  (cq :pointer)
  (tag :pointer))

(defvar *server-thread-pool* nil
  "Current server worker thread pool used by DISPATCH-REQUESTS when non-nil.")

(defun start-call-on-server (server &key (cq *completion-queue*) (use-call-cq nil))
  "Make gRPC SERVER call using completion queue CQ for notification and return
a call struct, or NIL if the server is shutting down or the call request failed.
When USE-CALL-CQ is true, binds the call to a dedicated pooled completion queue
so call operations can execute concurrently with subsequent calls on CQ."
  (cffi:with-foreign-object (tag :int)
    (let ((metadata (create-new-grpc-metadata-array))
          (call-details (create-grpc-call-details))
          (call-cq (if use-call-cq (acquire-pluck-cq) cq))
          (call nil))
      (unwind-protect
           (let ((c-call (grpc-server-request-call server call-details
                                                   metadata
                                                   call-cq
                                                   cq tag)))
             (unless (cffi:null-pointer-p c-call)
               (let ((method (get-call-method call-details))
                     (metadata-list (metadata-array-to-list metadata)))
                 (setf call
                       (make-call :c-call c-call
                                  :c-tag (cffi:null-pointer)
                                  :c-ops (cffi:null-pointer)
                                  :c-cq call-cq
                                  :owns-cq-p (not (null use-call-cq))
                                  :method-name method
                                  :ops-plist nil
                                  :is-server-call t
                                  :context (make-context :metadata metadata-list)))
                 call)))
        (when (and use-call-cq (null call))
          (release-pluck-cq call-cq))
        (metadata-destroy metadata)
        (call-details-destroy call-details)))))

(defun send-initial-metadata (call)
  "Send the GRPC_OP_SEND_INITIAL_METADATA from the server through a CALL"
  (declare (type call call))
  (let ((num-ops 1)
        (c-call (call-c-call call))
        (cq (call-completion-queue call)))
    (cffi:with-foreign-objects ((tag :int)
                                (ops '(:struct grpc-op)))
      (let ((ops-plist (prepare-ops ops :send-metadata t)))
        (declare (ignore ops-plist))
        (unwind-protect
             (let ((call-code (call-start-batch c-call ops num-ops tag)))
               (unless (eql call-code :grpc-call-ok)
                 (error 'grpc-call-error :call-error call-code))
               (let ((cqp-p (completion-queue-pluck cq tag)))
                 (when cqp-p (setf (call-initial-metadata-sent-p call) t))
                 cqp-p))
          (grpc-ops-clear ops num-ops))))))

(defun server-send-status (call &optional (status-code :grpc-status-ok)
                                          (with-recv-close nil)
                                          (status-details nil))
  "Send the GRPC_OP_SEND_STATUS_FROM_SERVER from the server through a CALL"
  (declare (type call call))
  (let ((num-ops (if with-recv-close 2 1))
        (c-call (call-c-call call))
        (cq (call-completion-queue call)))
    (cffi:with-foreign-objects ((tag :int)
                                (ops '(:struct grpc-op) 2))
      (let ((ops-plist (if with-recv-close
                           (prepare-ops ops
                                        :server-recv-close t
                                        :server-send-status status-code
                                        :server-send-status-details status-details)
                           (prepare-ops ops
                                        :server-send-status status-code
                                        :server-send-status-details status-details))))
        (declare (ignore ops-plist))
        (unwind-protect
             (let ((call-code (call-start-batch c-call ops num-ops tag)))
               (unless (eql call-code :grpc-call-ok)
                 (error 'grpc-call-error :call-error call-code))
               (completion-queue-pluck cq tag))
          (grpc-ops-clear ops num-ops))))))

(defun server-recv-close (call)
  "Send the GRPC_OP_RECV_STATUS_ON_CLIENT from the server through a CALL"
  (declare (type call call))
  (let ((num-ops 1)
        (c-call (call-c-call call))
        (cq (call-completion-queue call)))
    (cffi:with-foreign-objects ((tag :int)
                                (ops '(:struct grpc-op)))
      (let ((ops-plist (prepare-ops ops :server-recv-close t)))
        (declare (ignore ops-plist))
        (unwind-protect
             (let ((call-code (call-start-batch c-call ops num-ops tag)))
               (unless (eql call-code :grpc-call-ok)
                 (error 'grpc-call-error :call-error call-code))
               (completion-queue-pluck cq tag))
          (grpc-ops-clear ops num-ops))))))

(defun call-method-action (method call)
  "Invoke METHOD's action on CALL, reading and deserializing the request first
for non-input-streaming methods."
  (if (method-details-input-streaming-p method)
      (funcall (method-details-action method) call)
      (let* ((messages (or (receive-message call)
                           (error 'grpc-server-abort
                                  :status-code :grpc-status-internal
                                  :status-message "Failed to receive request message.")))
             (message (concatenate-byte-vectors messages))
             (deserialized-message (funcall (method-details-deserializer method) message)))
        (funcall (method-details-action method) deserialized-message call))))

(defun handle-server-call (call methods)
  "Executes the method handler in METHODS matching CALL, sends the response
and status, and frees CALL's C resources."
  (unwind-protect
       (let ((method (find (call-method-name call)
                           methods
                           :test #'string=
                           :key #'method-details-name)))
         (send-initial-metadata call)
         (flet ((finish-call (status-code &optional input-streaming-p status-message)
                  (unless (call-server-send-status-p call)
                    (setf (call-server-send-status-p call) t)
                    (server-send-status call status-code input-streaming-p status-message))
                  (unless input-streaming-p
                    (server-recv-close call))))
           (if method
               (let ((input-streaming-p (method-details-input-streaming-p method)))
                 (handler-case
                     (let ((response (call-method-action method call)))
                       (unless (method-details-output-streaming-p method)
                         (send-message call (funcall (method-details-serializer method)
                                                     response)))
                       (finish-call :grpc-status-ok input-streaming-p))
                   (grpc-server-abort (condition)
                     (finish-call (abort-status-code condition)
                                  input-streaming-p
                                  (abort-status-message condition)))))
               (finish-call :grpc-status-unimplemented))))
    (free-call-data call)))

(defun dispatch-requests (methods server &key (exit-count nil)
                                              (thread-pool *server-thread-pool*))
  "Block on the SERVER for a call then dispatch the call to the
proper method in METHODS based on the call method name. EXIT-COUNT
allows the caller to specify the number of times dispatch-call
can receive a call. When THREAD-POOL is a THREAD-POOL instance, each
accepted call is bound to its own pooled completion queue and executed
asynchronously on THREAD-POOL."
  (loop for calls-received from 0
        while (or (not exit-count)
                  (< calls-received exit-count))
        for call = (start-call-on-server server
                                         :use-call-cq (not (null thread-pool)))
        while call
        do (if thread-pool
               (let ((accepted-call call))
                 (unless (thread-pool-submit
                          thread-pool
                          (lambda ()
                            (handle-server-call accepted-call methods)))
                   (free-call-data accepted-call)))
               (handle-server-call call methods))))

(defun resolve-server-thread-pool (thread-pool num-threads)
  "Resolves THREAD-POOL (:DEFAULT, a THREAD-POOL instance, or NIL) and
NUM-THREADS (the maximum number of worker threads when THREAD-POOL is :DEFAULT)
and returns (VALUES POOL OWNS-POOL-P)."
  (etypecase thread-pool
    (null
     (values nil nil))
    ((eql :default)
     (etypecase num-threads
       (null
        (values nil nil))
       ((integer 1 *)
        (values (make-thread-pool :name "gRPC Server Worker"
                                  :max-threads num-threads)
                t))))
    (thread-pool
     (values thread-pool nil))))

(defun run-grpc-server (address methods
                        &key
                        (server-creds
                         (grpc-insecure-server-credentials-create))
                        (cq *completion-queue*)
                        (num-threads *default-thread-pool-size*)
                        (num-acceptors 1)
                        (thread-pool :default)
                        (dispatch-requests #'dispatch-requests))
  "Start a gRPC server.
Parameters
  ADDRESS: The address to run the server on.
  METHODS: The methods to start. Should be a list of method-details.
  SERVER-CREDS: Pointer to the gRPC server credentials.
  CQ: The completion queue to use.
  NUM-THREADS: Maximum number of worker threads in the default server thread
               pool (defaults to *DEFAULT-THREAD-POOL-SIZE*).
  NUM-ACCEPTORS: The number of acceptor threads calling dispatch-requests.
  THREAD-POOL: Worker thread pool configuration (:DEFAULT to create a pool of
               size NUM-THREADS, a THREAD-POOL instance, or NIL to run
               handlers synchronously on the acceptor threads).
  DISPATCH-REQUESTS: A function to use to dispatch calls.
                     Useful for debugging."
  (multiple-value-bind (worker-pool owns-pool-p)
      (resolve-server-thread-pool thread-pool num-threads)
    (let* ((server (start-server cq server-creds address))
           (thread-cqs (loop repeat num-acceptors
                             collect (c-grpc-completion-queue-create-for-pluck)))
           threads)
      (unwind-protect
           (progn
             (dolist (thread-cq thread-cqs)
               (register-server-completion-queue server thread-cq))

             (dolist (method methods)
               (format t "~s~%" (method-details-name method))
               (register-method server (method-details-name method) address))
             (run-server server server-creds)

             (loop for i from 0
                   for thread-cq in thread-cqs
                   do (let ((bound-cq thread-cq))
                        (push
                         (bordeaux-threads:make-thread
                          (lambda ()
                            (let ((*completion-queue* bound-cq)
                                  (*server-thread-pool* worker-pool))
                              (funcall dispatch-requests methods server)))
                          :name (format nil "Dispatch Request Thread ~a" i))
                         threads)))
             (loop while threads
                   for thread = (pop threads)
                   do (bordeaux-threads:join-thread thread))
             (when worker-pool
               (if owns-pool-p
                   (shutdown-thread-pool worker-pool :wait-p t)
                   (wait-for-thread-pool worker-pool))))
        (when owns-pool-p
          (shutdown-thread-pool worker-pool :wait-p nil))
        (cffi:with-foreign-object (tag :int)
          (shutdown-server server cq tag))
        (dolist (thread threads)
          (when (bordeaux-threads:thread-alive-p thread)
            (bordeaux-threads:join-thread thread)))
        (when owns-pool-p
          (shutdown-thread-pool worker-pool :wait-p t))
        (dolist (thread-cq thread-cqs)
          (destroy-completion-queue thread-cq))))))
