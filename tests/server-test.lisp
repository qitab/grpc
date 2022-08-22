;;; Copyright 2022 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

(defpackage #:grpc.test.server
  (:use #:cl
        #:clunit
        #:grpc)
  (:import-from #:grpc.test #:with-mocked-functions)
  (:export :run))

(in-package #:grpc.test.server)

(defsuite server-suite (grpc.test:root-suite))

(defun run (&key use-debugger)
  "Run all tests in the test suite.
Parameters
  USE-DEBUGGER: On assert failure bring up the debugger."
  (clunit:run-suite 'server-suite :use-debugger use-debugger
                                  :signal-condition-on-fail t))

(defun make-null-call ()
  "A helper function that returns a grpc::make-call object with the aprropriate parameters"
  (grpc::make-call :c-call (cffi:null-pointer)
                   :c-tag (cffi:foreign-alloc :int)
                   :c-ops (cffi:null-pointer)
                   :ops-plist nil))

(deftest test-server-check-error-success (server-suite)
  "Validate that send-initial-metadata, send-message, server-send-status,
server-recv-close, receive-message methods properly handle the scenario
 and check for errors when a call code of grpc-call-error is sent."
  (let* ((call-object (make-null-call))
         (message "Test")
         (text-result (flexi-streams:string-to-octets message)))
    (with-mocked-functions ((grpc::call-start-batch
                             (c-call ops num-ops tag)
                             (declare (ignore c-call ops num-ops tag))
                             :grpc-call-error))
      (assert-condition grpc::grpc-call-error
                        (grpc::send-initial-metadata call-object))
      (assert-condition grpc::grpc-call-error
                        (grpc::send-message call-object text-result))
      (assert-condition grpc::grpc-call-error
                        (grpc::server-send-status call-object))
      (assert-condition grpc::grpc-call-error
                        (grpc::server-recv-close call-object))
      (assert-condition grpc::grpc-call-error
                        (grpc::receive-message call-object)))))

(deftest test-server-return-true-success (server-suite)
  "Validate that send-initial-metadata, send-message,
server-send-status, server-recv-close, receive-message methods
properly handle the scenario and return true when a call code
 of grpc-call-ok is sent."
  (let* ((call-object (make-null-call))
         (message "Test")
         (text-result (flexi-streams:string-to-octets message)))
    (with-mocked-functions ((grpc::call-start-batch
                             (c-call ops num-ops tag)
                             (declare (ignore c-call ops num-ops tag))
                             :grpc-call-ok)
                            (grpc::completion-queue-pluck
                             (completion_queue tag)
                             (declare (ignore completion_queue tag))
                             t))
      (assert-true (grpc::send-initial-metadata call-object))
      (assert-true (grpc::send-message call-object text-result))
      (assert-true (grpc::server-send-status call-object))
      (assert-true (grpc::server-recv-close call-object)))))

(deftest test-server-receive-message-completion-queue-pluck-nil-success
    (server-suite)
  "Validate that receive-message method properly handles the scenario and
returns nil when a call code of grpc-call-ok and nil for completion-queue-pluck
 is sent"
  (let ((call-object (make-null-call)))
    (with-mocked-functions ((grpc::call-start-batch
                             (c-call ops num-ops tag)
                             (declare (ignore c-call ops num-ops tag))
                             :grpc-call-ok)
                            (grpc::completion-queue-pluck
                             (completion_queue tag)
                             (declare (ignore completion_queue tag))
                             nil))
      (assert-false (grpc::receive-message call-object)))))

(deftest test-server-receive-message-get-grpc-op-recv-message-null-pointer-success
    (server-suite)
  "Validate that receive-message method properly handles the scenario and
returns nil when a call code of grpc-call-ok, nil for completion-queue-pluck
 and null-pointer for get-grpc-op-recv-message is sent"
  (let ((call-object (make-null-call)))
    (with-mocked-functions ((grpc::call-start-batch
                             (c-call ops num-ops tag)
                             (declare (ignore c-call ops num-ops tag))
                             :grpc-call-ok)
                            (grpc::completion-queue-pluck
                             (completion_queue tag)
                             (declare (ignore completion_queue tag))
                             t)
                            (grpc::get-grpc-op-recv-message
                             (op index)
                             (declare (ignore op index))
                             (cffi:null-pointer)))
      (assert-false (grpc::receive-message call-object)))))
