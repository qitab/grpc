;;; Copyright 2021 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

;;;; cl-protobufs integration.
;;;; In a separate file so users can decide whether to use gRPC to
;;;; send bytes or to use the service functionality from cl-protobufs.


(in-package #:grpc)

(define-condition proto-call-error (error)
  ((call-error :initarg :call-error
               :initform nil
               :accessor call-error))
  (:report (lambda (condition stream)
             (format stream "PROTO CALL ERROR: ~A." (call-error condition)))))

;;; Tell the cl-protobufs method-call stubs who's in charge of RPC.
(setq cl-protobufs:*rpc-call-function* 'start-call)
(setq cl-protobufs:*rpc-streaming-client-function* 'handle-client-stream-call)
(setq cl-protobufs:*rpc-streaming-server-function* 'handle-server-stream-call)

(defun get-qualified-method-name (method)
  "Get the qualified METHOD name /service-name/method-name for a method
given a cl-protobufs method-descriptor."
  (let ((service-name (proto:proto-service-name method))
        (rpc-name (proto:proto-name method))
        ;; Package name is needed for service name
        ;; but not provided directly in the method, so take
        ;; it from the qualified name.
        (package-name
         (subseq (proto:proto-qualified-name method) 0
                 (position #\. (proto:proto-qualified-name method) :from-end t))))
    (concatenate 'string "/" package-name "." service-name "/" rpc-name)))

(defgeneric start-call (channel method request response &key callback timeout)
  (:documentation
   "Starts a gRPC call for METHOD.

Parameters:
    CHANNEL is the channel to send a call over.
    METHOD is the cl-protobuf method we wish to call.
    REQUEST is the proto message to send.
    RESPONSE is not supported.
    CALLBACK is not currently supported.
    TIMEOUT is the timeout for the call in seconds."))

(defmethod start-call (channel method request response &key callback (timeout -1.0d0))
  (assert (not (or callback response)) nil "CALLBACK and RESPONSE args not supported.")
  (let* ((qualified-method-name (get-qualified-method-name method))
         (output-type (proto:proto-output-type method))
         (server-stream (proto:proto-output-streaming-p method))
         (client-stream (proto:proto-input-streaming-p method))
         (bytes (if client-stream
                    (mapcar #'proto:serialize-to-bytes request)
                    (proto:serialize-to-bytes request)))
         (response (grpc-call channel qualified-method-name bytes
                              server-stream client-stream timeout)))
    (flet ((deserialize-result (bytes)
             (proto:deserialize-from-bytes
              output-type
              (apply #'concatenate 'proto:byte-vector bytes))))
      (if server-stream
          (mapcar #'deserialize-result response)
          (deserialize-result response)))))

(defstruct (client-proto-call (:include call)))
(defstruct (server-proto-call (:include call)))

(define-condition grpc-server-abort (error)
  ((status-code :initarg :status-code
                :initform :grpc-status-unknown
                :accessor abort-status-code)
   (status-message :initarg :status-message
                   :initform ""
                   :accessor abort-status-message))
  (:report (lambda (condition stream)
             (format stream "gRPC Server Abort: ~A (~A)"
                     (abort-status-message condition)
                     (abort-status-code condition)))))

(defun abort-server-stream (status-code &optional (status-message ""))
  "Aborts a server streaming call with STATUS-CODE and STATUS-MESSAGE."
  (error 'grpc-server-abort :status-code status-code :status-message status-message))

(defgeneric stream-send (call message)
  (:documentation "Sends a Protobuf message over a streaming CALL (client or server)."))

(defmethod stream-send ((call server-proto-call) message)
  (when (call-server-send-status-p call)
    (error 'proto-call-error :call-error "Tried to send message after server finished."))
  (unless (call-server-stream-p call)
    (error 'proto-call-error
           :call-error "Tried to send multiple messages from a non-streaming server."))
  (send-message call (proto:serialize-to-bytes message)))

(defmethod stream-send ((call client-proto-call) message)
  (when (call-client-stream-closed-p call)
    (error 'proto-call-error :call-error "Tried to send message on closed stream"))
  (when (call-call-cleaned-up-p call)
    (error 'proto-call-error :call-error "Tried to send message with call cleaned up."))
  (when (and (not (call-client-stream-p call))
             (call-initial-message-sent-p call))
    (error 'proto-call-error
           :call-error "Tried to send multiple messages from a non-streaming client."))
  (setf (call-initial-message-sent-p call) t)
  (send-message call (proto:serialize-to-bytes message)))

(defgeneric stream-receive (call)
  (:documentation "Receives a Protobuf message from a streaming CALL (client or server)."))

(defmethod stream-receive ((call server-proto-call))
  (when (call-server-send-status-p call)
    (error 'proto-call-error :call-error "Tried to receive message after server finished."))
  (unless (call-client-stream-p call)
    (error 'proto-call-error
           :call-error "Tried to receive multiple messages from a non-streaming client."))
  (let ((messages (receive-message call)))
    (when messages
      (proto:deserialize-from-bytes
       (call-input-type call)
       (apply #'concatenate 'proto:byte-vector messages)))))

(defmethod stream-receive ((call client-proto-call))
  (when (call-call-cleaned-up-p call)
    (error 'proto-call-error
           :call-error "Tried to receive message with call cleaned up."))
  (unless (call-initial-message-sent-p call)
    (error 'proto-call-error
           :call-error "Tried to receive message before sending a message."))
  (let ((messages (receive-message call)))
    (if messages
        (proto:deserialize-from-bytes
         (call-output-type call)
         (apply #'concatenate 'proto:byte-vector messages))
        (progn
          (check-server-status call)
          nil))))

(defgeneric stream-close (call)
  (:documentation "Half-closes a streaming CALL (client or server)."))

(defmethod stream-close ((call server-proto-call))
  (setf (call-client-stream-closed-p call) t)
  (server-recv-close call))

(defmethod stream-close ((call client-proto-call))
  (setf (call-client-stream-closed-p call) t)
  (client-close call))

(defgeneric stream-cleanup (call)
  (:documentation "Cleans up a client streaming CALL."))

(defmethod stream-cleanup ((call client-proto-call))
  (unless (call-client-stream-closed-p call)
    (error 'proto-call-error
           :call-error "Tried to cleanup call before closing the call."))
  (free-call-data call))

(defmacro do-stream-receive ((message-var call) &body body)
  "Repeatedly receives messages from CALL and binds them to MESSAGE-VAR until EOF (nil)."
  (let ((call-var (gensym "CALL")))
    `(let ((,call-var ,call))
       (loop for ,message-var = (stream-receive ,call-var)
             while ,message-var
             do (progn ,@body)))))

(defgeneric handle-client-stream-call (type &key channel method request call timeout)
  (:documentation
   "Dispatch for different stream call types."))

(defmethod handle-client-stream-call ((type (eql :start))
                                      &key channel method request call (timeout -1.0d0))
  (declare (ignore type request call))
  (let* ((qualified-method-name (get-qualified-method-name method))
         (call (start-grpc-call channel qualified-method-name timeout)))
    (make-client-proto-call
     :c-call (call-c-call call)
     :c-tag (call-c-tag call)
     :c-ops (call-c-ops call)
     :ops-plist (call-ops-plist call)
     :server-stream-p (proto:proto-output-streaming-p method)
     :client-stream-p (proto:proto-input-streaming-p method)
     :input-type (proto:proto-input-type method)
     :output-type (proto:proto-output-type method)
     :is-server-call nil)))

(defmethod handle-client-stream-call ((type (eql :send))
                                      &key channel method request call timeout)
  (declare (ignore type channel method timeout))
  (stream-send call request))

(defmethod handle-client-stream-call ((type (eql :receive))
                                      &key channel method request call timeout)
  (declare (ignore type channel method request timeout))
  (stream-receive call))

(defmethod handle-client-stream-call ((type (eql :close))
                                      &key channel method request call timeout)
  (declare (ignore type channel method request timeout))
  (stream-close call))

(defmethod handle-client-stream-call ((type (eql :cleanup))
                                      &key channel method request call timeout)
  (declare (ignore type channel method request timeout))
  (stream-cleanup call))

(defgeneric handle-server-stream-call (type &key channel method request call timeout)
  (:documentation
   "Dispatch for different server stream call types."))

(defmethod handle-server-stream-call ((type (eql :receive))
                                      &key channel method request call timeout)
  (declare (ignore type channel method request timeout))
  (stream-receive call))

(defmethod handle-server-stream-call ((type (eql :send))
                                      &key channel method request call timeout)
  (declare (ignore type channel method timeout))
  (stream-send call request))

(defmethod handle-server-stream-call ((type (eql :receive-close))
                                      &key channel method request call timeout)
  (declare (ignore type channel method request timeout))
  (stream-close call))

(defmethod handle-server-stream-call ((type (eql :send-status))
                                      &key channel method request call timeout)
  (declare (ignore type channel method request timeout))
  (setf (call-server-send-status-p call) t)
  (server-send-status call))

(defun run-grpc-proto-server (address service-name
                              &key
                              (server-creds
                               (grpc-insecure-server-credentials-create))
                              (cq grpc::*completion-queue*)
                              (num-threads 1)
                              (dispatch-requests #'dispatch-requests))
  "Start a gRPC server using protocol buffers.
Parameters
  ADDRESS: The address to run the server on.
  SERVICE-NAME: The symbol naming the service to run.
  SERVER-CREDS: Pointer to the gRPC server credentials.
  CQ: The completion queue to use.
  NUM-THREADS: The number of threads to have running.
  DISPATCH-CALL: A function to use to dispatch calls.
                 Useful for debugging."
  (let* ((service (proto:find-service-descriptor service-name))
         method-details-list)
    (dolist (method (proto:proto-methods service))
      (let ((istream-p (proto:proto-input-streaming-p method))
            (ostream-p (proto:proto-output-streaming-p method)))
        (push
         (make-method-details
          :name (get-qualified-method-name method)
          :serializer #'proto:serialize-to-bytes
          :deserializer (lambda (bytes)
                          (proto:deserialize-from-bytes
                           (proto:proto-input-type method)
                           bytes))
          :action
          (if istream-p
              (lambda (call)
                (let ((pcall (make-server-proto-call
                              :c-call (call-c-call call)
                              :c-tag (call-c-tag call)
                              :c-ops (call-c-ops call)
                              :ops-plist (call-ops-plist call)
                              :server-stream-p ostream-p
                              :client-stream-p istream-p
                              :input-type (proto:proto-input-type method)
                              :output-type (proto:proto-output-type method)
                              :is-server-call (call-is-server-call call))))
                  (funcall (proto:proto-server-stub method) pcall)))
              (lambda (message call)
                (let ((pcall (make-server-proto-call
                              :c-call (call-c-call call)
                              :c-tag (call-c-tag call)
                              :c-ops (call-c-ops call)
                              :ops-plist (call-ops-plist call)
                              :server-stream-p ostream-p
                              :client-stream-p istream-p
                              :input-type (proto:proto-input-type method)
                              :output-type (proto:proto-output-type method)
                              :is-server-call (call-is-server-call call))))
                  (funcall (proto:proto-server-stub method) message pcall))))
          :input-streaming-p istream-p
          :output-streaming-p ostream-p)
         method-details-list)))
    (run-grpc-server address method-details-list
                     :server-creds server-creds
                     :cq cq
                     :num-threads num-threads
                     :dispatch-requests dispatch-requests)))

(cl:export '(run-grpc-proto-server stream-send stream-receive stream-close
             stream-cleanup do-stream-receive grpc-server-abort abort-server-stream))
