;; Copyright 2016-2021 Google LLC
;;
;; Use of this source code is governed by an MIT-style
;; license that can be found in the LICENSE file or at
;; https://opensource.org/licenses/MIT.

;;  A wrapper for using gRPC in Common Lisp.

(defpackage #:grpc
  (:use #:common-lisp)
  (:local-nicknames
   (#:proto-impl #:cl-protobufs.implementation)
   (#:proto #:cl-protobufs))
  (:export
   ;; Client and Lifecycle Functions
   #:init-grpc
   #:shutdown-grpc
   #:with-insecure-channel
   #:with-ssl-channel
   #:grpc-call
   #:grpc-async-call
   #:async-call
   #:async-call-p
   #:async-call-ready-p
   #:async-call-wait
   #:check-server-status
   #:with-client-stream
   #:stream-send
   #:stream-receive
   #:stream-close
   #:stream-cleanup
   #:do-stream-receive
   ;; Context
   #:context
   #:make-context
   #:context-p
   #:context-deadline
   #:context-metadata
   #:call-context
   ;; Server and Method Details
   #:run-grpc-server
   #:run-grpc-proto-server
   #:dispatch-requests
   #:grpc-server-abort
   #:abort-status-code
   #:abort-status-message
   #:abort-server-stream
   #:grpc-insecure-server-credentials-create
   #:method-details
   #:make-method-details
   #:method-details-p
   #:method-details-name
   #:method-details-serializer
   #:method-details-deserializer
   #:method-details-action
   #:method-details-server-stream
   #:method-details-client-stream
   #:method-details-input-streaming-p
   #:method-details-output-streaming-p
   ;; Conditions
   #:grpc-call-error
   #:proto-call-error
   #:call-error
   #:call-error-status-message))
