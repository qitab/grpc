;;; Copyright 2022 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

;;  A simple integration test for gRPC Client and Server in Common Lisp

(defpackage #:grpc.test.proto-server
  (:use #:cl
        #:clunit
        #:grpc)
  (:local-nicknames
   (#:ut #:cl-protobufs.lisp.grpc.unit-testing)
   (#:ut-rpc #:cl-protobufs.lisp.grpc.unit-testing-rpc))
  (:export :run))

(in-package #:grpc.test.proto-server)

(defsuite proto-server-suite (grpc.test:root-suite))

(defun run (&key use-debugger)
  "Run all tests in the test suite.
Parameters
  USE-DEBUGGER: On assert failure bring up the debugger."
  (clunit:run-suite 'proto-server-suite :use-debugger use-debugger
                                        :signal-condition-on-fail t))

(defmethod ut-rpc::say-hello ((request ut:hello-request) rpc)
  (declare (ignore rpc))
  (ut:make-hello-reply
   :message
   (concatenate 'string
                (ut:hello-request.name request)
                " Back")))

(defun run-server (sem hostname port-number)
  (grpc::run-grpc-proto-server
   (concatenate 'string
                hostname ":"
                (write-to-string port-number))
   'ut:greeter
   :dispatch-requests
   (lambda (method server)
     (bordeaux-threads:signal-semaphore sem)
     (grpc::dispatch-requests method server :exit-count 1))))

(deftest test-client-server-integration-success (proto-server-suite)
  ;; init
  (grpc:init-grpc)
  (unwind-protect
       (let* ((expected-client-response "Hello World Back")
              (hostname "localhost")
              (port-number 8000)
              (sem (bordeaux-threads:make-semaphore))
              (thread (bordeaux-threads:make-thread
                       (lambda () (run-server sem hostname port-number)))))

         (bordeaux-threads:wait-on-semaphore sem)

         (grpc:with-insecure-channel
             (channel (concatenate 'string hostname ":"
                                   (write-to-string port-number)))
           ;; Unary streaming
           (let* ((message (ut:make-hello-request :name "Hello World"))
                  (response (ut-rpc:call-say-hello channel message)))
             (assert-true (string= (ut:hello-reply.message response)
                                   expected-client-response))))
         (bordeaux-threads:join-thread thread))
    (grpc:shutdown-grpc)))
