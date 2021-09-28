;;; Copyright 2021 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

;;;; cl-protobufs integration.
;;;; In a separate file so users can decide whether to use gRPC to
;;;; send bytes or to use the service functionality from cl-protobufs.


(in-package #:grpc)

;; Tell the Protobufs-generated stubs who's in charge of RPC
(setq proto-impl:*rpc-package* (find-package "GRPC")
      proto-impl:*rpc-call-function* 'start-call)

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
  (let* ((service-name (proto:proto-service-name method))
         (rpc-name (proto:proto-name method))
         ;; Package name is needed for service name
         ;; but not provided directly in the method, so take
         ;; it from the qualified name.
         (package-name
          (subseq (proto:proto-qualified-name method) 0
                  (position #\. (proto:proto-qualified-name method))))
         (qualified-method-name
          (concatenate 'string "/" package-name "." service-name "/" rpc-name))
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
              (apply #' concatenate 'proto:byte-vector bytes))))
      (if server-stream
          (mapcar #'deserialize-result response)
          (deserialize-result response)))))
