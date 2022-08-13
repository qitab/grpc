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

(defun run-server (expected-server-response sem hostname methodnames port-number)
  (declare (ignore methodnames expected-server-response))
  (grpc::run-grpc-server
   (concatenate 'string
                hostname ":"
                (write-to-string port-number))
   (list
    (list
     "xyz" (lambda (message)
             (let ((actual-server-response
                    (flexi-streams:octets-to-string
                     message
                     :external-format
                     :utf-8)))
               (format t "~% response: ~A ~%" actual-server-response)
               ;; (assert-true (string=
               ;;               expected-server-response
               ;;               actual-server-response))
               (let* ((message (concatenate 'string
                                            actual-server-response
                                            " Back"))
                      (octet-response
                       (flexi-streams:string-to-octets message)))
                 octet-response)))))
   grpc::*completion-queue*
   :run-methods
   (lambda (method server)
     (bordeaux-threads:signal-semaphore sem)
     (grpc::run-methods method server)
     )
   :exit-count 1))


(deftest test-client-server-integration-success (server-suite)
  ;; init
  (grpc:init-grpc)
  (unwind-protect
       (let* ((expected-server-response "Hello World")
              (expected-client-response "Hello World Back")
              (hostname "localhost")
              (methodnames '("xyz"))
              (port-number 8000)
              (sem (bordeaux-threads:make-semaphore))
              (thread (bordeaux-threads:make-thread
                       (lambda () (run-server expected-server-response
                                              sem hostname methodnames port-number)))))
         (bordeaux-threads:wait-on-semaphore sem)
         (grpc:with-insecure-channel
             (channel
              (concatenate 'string hostname ":" (write-to-string port-number)))
           (let* ((message "Hello World")
                  (response (grpc:grpc-call channel (first methodnames)
                                            (flexi-streams:string-to-octets message)
                                            nil nil))
                  (actual-client-response (flexi-streams:octets-to-string
                                           (car response))))
             (assert-true (string= actual-client-response
                                   expected-client-response))
             (bordeaux-threads:join-thread thread)))))
  (grpc:shutdown-grpc)
  )
