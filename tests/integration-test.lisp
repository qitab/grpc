;;; Copyright 2022 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

;;  A simple integration test for gRPC Client and Server in Common Lisp

(defpackage #:grpc.test.server
  (:use #:cl
        #:clunit
        #:grpc)
  (:export :run))

(in-package #:grpc.test.server)

(defsuite server-suite (grpc.test:root-suite))

(defun run (&key use-debugger)
  "Run all tests in the test suite.
Parameters
  USE-DEBUGGER: On assert failure bring up the debugger."
  (clunit:run-suite 'server-suite :use-debugger use-debugger
                                  :signal-condition-on-fail t))


(defun run-server (expected-server-response sem hostname methodname port-number)
  (let* ((server-address (concatenate 'string hostname ":" (write-to-string port-number)))
         (server (grpc::start-server grpc::*completion-queue* server-address methodname))
         (call-object (progn (bordeaux-threads:signal-semaphore sem)
                             (grpc::start-call-on-server server)))
         (message (grpc::receive-message-from-client call-object))
         (result (apply #'concatenate '(array (unsigned-byte 8) (*)) message))
         (actual-server-response (flexi-streams:octets-to-string result :external-format :utf-8)))
    (assert-true (string= expected-server-response actual-server-response))
    (let* ((message "Hello World Back")
           (text-result (flexi-streams:string-to-octets message)))
      (grpc::send-message-to-client call-object text-result))))


(deftest test-client-server-integration-success (server-suite)
  (google:init)
  (grpc:init-grpc)
  (unwind-protect
       (let* ((expected-server-response "Hello World")
              (expected-client-response "Hello World Back")
              (hostname "localhost")
              (methodname "xyz")
              (port-number 8000)
              (sem (bordeaux-threads:make-semaphore))
              (thread (bordeaux-threads:make-thread
                       (lambda () (run-server expected-server-response sem hostname methodname port-number)))))
         (bordeaux-threads:wait-on-semaphore sem)
         (grpc:with-loas2-channel
             (channel
              ((concatenate 'string hostname ":" (write-to-string port-number)) ()))
           (let* (
                  (message "Hello World")
                  (response (grpc:grpc-call channel methodname
                                            (flexi-streams:string-to-octets message) nil nil))
                  (actual-client-response (flexi-streams:octets-to-string (car response))))
             (assert-true (string=  actual-client-response expected-client-response))
             (bordeaux-threads:join-thread thread)))))
  (grpc:shutdown-grpc))
