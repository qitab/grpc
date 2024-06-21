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

(defun run-server (sem hostname method-name port-number)
  (grpc::run-grpc-server
   (concatenate 'string
                hostname ":"
                (write-to-string port-number))
   (list
    (grpc::make-method-details
     :name method-name
     :serializer #'flexi-streams:string-to-octets
     :deserializer
     (lambda (message)
       (flexi-streams:octets-to-string
        message
        :external-format
        :utf-8))
     :action
     (lambda (message call)
       (declare (ignore call))
       (format t "~% response: ~A ~%" message)
       (concatenate 'string
                    message
                    " Back"))))
   :dispatch-requests
   (lambda (method server)
     (bordeaux-threads:signal-semaphore sem)
     (grpc::dispatch-requests method server :exit-count 1))))

(deftest test-client-server-integration-success (server-suite)
  ;; init
  (grpc:init-grpc)
  (unwind-protect
       (let* ((expected-client-response "Hello World Back")
              (hostname "localhost")
              (method-name "xyz")
              (port-number 8000)
              (sem (bordeaux-threads:make-semaphore))
              (thread (bordeaux-threads:make-thread
                       (lambda () (run-server sem hostname method-name
                                              port-number)))))
         (bordeaux-threads:wait-on-semaphore sem)
         (grpc:with-insecure-channel
             (channel
              (concatenate 'string hostname ":" (write-to-string port-number)))
           (let* ((message "Hello World")
                  (response (grpc:grpc-call channel method-name
                                            (flexi-streams:string-to-octets message)
                                            nil nil))
                  (actual-client-response (flexi-streams:octets-to-string
                                           (car response))))
             (assert-true (string=  actual-client-response expected-client-response))
             (bordeaux-threads:join-thread thread)))))
  (grpc:shutdown-grpc))

(deftest test-client-server-deadline (server-suite)
  (grpc:init-grpc)
  (unwind-protect
       (let* ((expected-client-response "Hello World Back")
              (hostname "localhost")
              (method-name "xyz")
              (port-number 8000)
              (sem (bordeaux-threads:make-semaphore))
              (thread (bordeaux-threads:make-thread
                       (lambda () (run-server sem hostname method-name
                                              port-number)))))
         (bordeaux-threads:wait-on-semaphore sem)
         (grpc:with-insecure-channel
             (channel
              (concatenate 'string hostname ":" (write-to-string port-number)))
           (let* ((grpc::*call-deadline* 3)
                  (message "Hello World")
                  (response (grpc:grpc-call channel method-name
                                            (flexi-streams:string-to-octets message)
                                            nil nil))
                  (actual-client-response (flexi-streams:octets-to-string
                                           (car response))))
             (assert-true (string=  actual-client-response expected-client-response))
             (bordeaux-threads:join-thread thread)))))
  (grpc:shutdown-grpc))
