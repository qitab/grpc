;;; Copyright 2021 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

(defpackage #:grpc.test.protobuf-integration
  (:use #:cl
        #:clunit
        #:cl-protobufs
        #:grpc)
  (:import-from #:grpc.test #:with-mocked-functions)
  (:local-nicknames (#:test-proto #:cl-protobufs.lisp.grpc.unit-testing))
  (:export :run))

(in-package #:grpc.test.protobuf-integration)

(defsuite protobuf-integration-suite (grpc.test:root-suite))

(defun run (&key use-debugger)
  "Run all tests in the test suite.
Parameters
  USE-DEBUGGER: On assert failure bring up the debugger."
  (clunit:run-suite 'protobuf-integration-suite :use-debugger use-debugger
                                                :signal-condition-on-fail t))

(deftest test-get-qualified-method-name (protobuf-integration-suite)
  "Validates the qualified name can be parsed from the provided method descriptor."
  (let* ((method-descriptor (make-instance 'cl-protobufs:method-descriptor
                                           :service-name "Greeter"
                                           :name "SayHello"
                                           :qualified-name "lisp.grpc.test.SayHello"))
         (expected-qualified-method-name "/lisp.grpc.test.Greeter/SayHello")
         (qualified-method-name (grpc::get-qualified-method-name method-descriptor)))
    (assert-true (string= expected-qualified-method-name qualified-method-name))))

(deftest test-start-call-unary-rpc (protobuf-integration-suite)
  "Validate the start-call method properly handles the scenario in which a single request is sent to
the server and a single response is returned."
  (let ((request (test-proto:make-hello-request :name "Neo"))
        (expected-response (test-proto:make-hello-reply :message "Hello, Neo"))
        (method (make-instance 'cl-protobufs:method-descriptor
                               :service-name "Greeter"
                               :name "SayHello"
                               :qualified-name "lisp.grpc.test.SayHello"
                               :output-type 'test-proto:hello-reply
                               :output-streaming nil
                               :input-streaming nil))
        (qualified-method-name "/lisp.grpc.test.Greeter/SayHello"))
    (with-mocked-functions ((grpc-call
                             (channel
                              service-method-name
                              bytes-to-send
                              server-stream
                              client-stream)
                             ;; No need to allocate extra memory for a channel that won't be used
                             ;; since we're mocking the underlying call.
                             (declare (ignore channel))
                             (assert-true (string= service-method-name qualified-method-name))
                             (assert-equalp bytes-to-send
                                            (cl-protobufs:serialize-to-bytes request))
                             (assert-eq server-stream nil)
                             (assert-eq client-stream nil)
                             (list (cl-protobufs:serialize-to-bytes expected-response))))
      (let ((actual-response (grpc::start-call "channel" method request nil)))
        (assert-equalp actual-response expected-response)))))

(deftest test-start-call-server-streaming-rpc (protobuf-integration-suite)
  "Validate the start-call method properly handles the scenario in which a single request is sent to
the server and a stream of responses are returned."
  (let ((request (test-proto:make-hello-request :name "Neo" :num-responses 3))
        (expected-response (list (test-proto:make-hello-reply :message "Hello, Neo 0")
                                 (test-proto:make-hello-reply :message "Hello, Neo 1")
                                 (test-proto:make-hello-reply :message "Hello, Neo 2")))
        (method (make-instance 'cl-protobufs:method-descriptor
                               :service-name "Greeter"
                               :name "SayHelloServerStream"
                               :qualified-name "lisp.grpc.test.SayHelloServerStream"
                               :output-type 'test-proto:hello-reply
                               :output-streaming t
                               :input-streaming nil))
        (qualified-method-name "/lisp.grpc.test.Greeter/SayHelloServerStream"))
    (with-mocked-functions ((grpc-call
                             (channel
                              service-method-name
                              bytes-to-send
                              server-stream
                              client-stream)
                             ;; No need to allocate extra memory for a channel that won't be used
                             ;; since we're mocking the underlying call.
                             (declare (ignore channel))
                             (assert-true (string= service-method-name qualified-method-name))
                             (assert-equalp bytes-to-send
                                            (cl-protobufs:serialize-to-bytes request))
                             (assert-eq server-stream t)
                             (assert-eq client-stream nil)
                             (loop for message in expected-response
                                   collect (list (cl-protobufs:serialize-to-bytes message)))))
      (let ((actual-response (grpc::start-call "channel" method request nil)))
        (assert-equalp actual-response expected-response)))))

(deftest test-start-call-client-streaming-rpc (protobuf-integration-suite)
  "Validate the start-call method properly handles the scenario in which a stream of requests are
sent to the server and a single response is returned."
  (let ((request (list (test-proto:make-hello-request :name "Neo")
                       (test-proto:make-hello-request :name "Morpheus")
                       (test-proto:make-hello-request :name "Trinity")))
        (expected-response (test-proto:make-hello-reply :message "Hello, Neo Morpheus Trinity"))
        (method (make-instance 'cl-protobufs:method-descriptor
                               :service-name "Greeter"
                               :name "SayHelloClientStream"
                               :qualified-name "lisp.grpc.test.SayHelloClientStream"
                               :output-type 'test-proto:hello-reply
                               :output-streaming nil
                               :input-streaming t))
        (qualified-method-name "/lisp.grpc.test.Greeter/SayHelloClientStream"))
    (with-mocked-functions ((grpc-call
                             (channel
                              service-method-name
                              bytes-to-send
                              server-stream
                              client-stream)
                             ;; No need to allocate extra memory for a channel that won't be used
                             ;; since we're mocking the underlying call.
                             (declare (ignore channel))
                             (assert-true (string= service-method-name qualified-method-name))
                             (assert-equalp bytes-to-send
                                            (mapcar #'cl-protobufs:serialize-to-bytes request))
                             (assert-eq server-stream nil)
                             (assert-eq client-stream t)
                             (list (cl-protobufs:serialize-to-bytes expected-response))))
      (let ((actual-response (grpc::start-call "channel" method request nil)))
        (assert-equalp actual-response expected-response)))))

(deftest test-start-call-bidirectional-streaming (protobuf-integration-suite)
  "Validate the start-call method properly handles the scenario in which a stream of requests are
sent to the server and a stream of responses are returned."
  (let ((request (list (test-proto:make-hello-request :name "Neo" :num-responses 1)
                       (test-proto:make-hello-request :name "Morpheus" :num-responses 2)
                       (test-proto:make-hello-request :name "Trinity" :num-responses 1)))
        (expected-response (list (test-proto:make-hello-reply :message "Hello, Neo 0")
                                 (test-proto:make-hello-reply :message "Hello, Morpheus 1")
                                 (test-proto:make-hello-reply :message "Hello, Neo 1")
                                 (test-proto:make-hello-reply :message "Hello, Trinity 0")
                                 (test-proto:make-hello-reply :message "Hello, Morpheus 0")))
        (method (make-instance 'cl-protobufs:method-descriptor
                               :service-name "Greeter"
                               :name "SayHelloBidirectionalStream"
                               :qualified-name "lisp.grpc.test.SayHelloBidirectionalSream"
                               :output-type 'test-proto:hello-reply
                               :output-streaming t
                               :input-streaming t))
        (qualified-method-name "/lisp.grpc.test.Greeter/SayHelloBidirectionalStream"))
    (with-mocked-functions ((grpc-call
                             (channel
                              service-method-name
                              bytes-to-send
                              server-stream
                              client-stream)
                             ;; No need to allocate extra memory for a channel that won't be used
                             ;; since we're mocking the underlying call.
                             (declare (ignore channel))
                             (assert-true (string= service-method-name qualified-method-name))
                             (assert-equalp bytes-to-send
                                            (mapcar #'cl-protobufs:serialize-to-bytes request))
                             (assert-eq server-stream t)
                             (assert-eq client-stream t)
                             (loop for message in expected-response
                                   collect (list (cl-protobufs:serialize-to-bytes message)))))
      (let ((actual-response (grpc::start-call "channel" method request nil)))
        (assert-equalp actual-response expected-response)))))
