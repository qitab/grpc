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

(defgeneric start-call (channel method request response &key callback)
  (:documentation
   "Starts a gRPC call for METHOD.

Parameters:
    CHANNEL is the channel to send a call over.
    METHOD is the cl-protobuf method we wish to call.
    REQUEST is the proto message to send.
    RESPONSE is not supported.
    CALLBACK is not currently supported."))

(defmethod start-call (channel method request response &key callback)
  (assert (not (or callback response)) nil "CALLBACK and RESPONSE args not supported.")
  (let* ((qualified-method-name (get-qualified-method-name method))
         (output-type (proto:proto-output-type method))
         (server-stream (proto:proto-output-streaming-p method))
         (client-stream (proto:proto-input-streaming-p method))
         (bytes (if client-stream
                    (mapcar #'proto:serialize-to-bytes request)
                    (proto:serialize-to-bytes request)))
         (response (grpc-call channel qualified-method-name bytes
                              server-stream client-stream)))
    (flet ((deserialize-result (bytes)
             (proto:deserialize-from-bytes
              output-type
              (apply #'concatenate 'proto:byte-vector bytes))))
      (if server-stream
          (mapcar #'deserialize-result response)
          (deserialize-result response)))))

(defstruct (proto-call (:include call))
  (output-type nil :type symbol)
  (client-stream-closed-p nil :type boolean)
  (call-cleaned-up-p nil :type boolean)
  (client-stream-p nil :type boolean)
  (server-stream-p nil :type boolean)
  (initial-message-sent-p nil :type boolean))

(defgeneric handle-client-stream-call (type &key channel method request call)
  (:documentation
   "Dispatch for different stream call types.

Parameters:
    TYPE is the type of call this should be..
    CHANNEL is the channel to send a call over.
    METHOD is the cl-protobuf method-descriptor for the method we wish to call.
    REQUEST is the proto message to send.
    CALL contains a proto-call object."))

(defmethod handle-client-stream-call ((type (eql :start)) &key channel method request call)
  "Start a gRPC call over a CHANNEL to a specific rpc METHOD.
Ignores TYPE REQUEST and CALL."
  (declare (ignore type request call))
  (let* ((qualified-method-name (get-qualified-method-name method))
         (call (start-grpc-call channel qualified-method-name)))
    (make-proto-call
     :c-call (call-c-call call)
     :c-tag (call-c-tag call)
     :c-ops (call-c-ops call)
     :ops-plist (call-ops-plist call)
     :server-stream-p (proto:proto-output-streaming-p method)
     :client-stream-p (proto:proto-input-streaming-p method)
     :output-type (proto:proto-output-type method))))

(defmethod handle-client-stream-call ((type (eql :send)) &key channel method request call)
  "Send a REQUEST over a CALL.
Ignores TYPE CHANNEL and METHOD."
  (declare (ignore type channel method))
  (when (proto-call-client-stream-closed-p call)
    (error 'proto-call-error :call-error "Tried to send message on closed stream"))
  (when (proto-call-call-cleaned-up-p call)
    (error 'proto-call-error :call-error "Tried to send message with call cleaned up."))
  (when (and (not (proto-call-client-stream-p call))
             (proto-call-initial-message-sent-p call))
    (error 'proto-call-error :call-error
           "Tried to send multiple messages from a non-streaming client."))
  (setf (proto-call-initial-message-sent-p call) t)
  (send-message call (proto:serialize-to-bytes request)))

(defmethod handle-client-stream-call ((type (eql :receive)) &key channel method request call)
  "Receive a message from a CHANNEL.
Ignores TYPE CHANNEL METHOD and REQUEST."
  (declare (ignore type channel method request))
  (when (proto-call-call-cleaned-up-p call)
    (error 'proto-call-error :call-error "Tried to received message with call cleaned up."))
  (unless (proto-call-initial-message-sent-p call)
    (error 'proto-call-error :call-error "Tried to received message before sending a message."))
  (proto:deserialize-from-bytes
   (proto-call-output-type call)
   (apply #'concatenate 'proto:byte-vector (receive-message call))))

(defmethod handle-client-stream-call ((type (eql :close)) &key channel method request call)
  "Close a CALL from the client-side. Server side remains open.
Ignores TYPE CHANNEL METHOD and REQUEST."
  (declare (ignore type channel method request))
  (setf (proto-call-client-stream-closed-p call) t)
  (client-close call))

(defmethod handle-client-stream-call ((type (eql :cleanup)) &key channel method request call)
  "Cleanup the CALL data stored in a proto-call structure.
Ignores TYPE CHANNEL METHOD and REQUEST."
  (declare (ignore type channel method request))
  (unless (proto-call-client-stream-closed-p call)
    (error 'proto-call-error :call-error "Tried to cleanup call before closing the call."))
  (free-call-data call))
