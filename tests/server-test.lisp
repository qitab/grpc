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
      (assert-condition grpc:grpc-call-error
                        (grpc::send-initial-metadata call-object))
      (assert-condition grpc:grpc-call-error
                        (grpc::send-message call-object text-result))
      (assert-condition grpc:grpc-call-error
                        (grpc::server-send-status call-object))
      (assert-condition grpc:grpc-call-error
                        (grpc::server-recv-close call-object))
      (assert-condition grpc:grpc-call-error
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
  "Validate that receive-message method properly handles the scenario, clears
receive-op, and returns nil when a call code of grpc-call-ok and nil for
completion-queue-pluck is sent."
  (let ((call-object (make-null-call))
        (ops-cleared-p nil)
        (orig-ops-clear #'grpc::grpc-ops-clear))
    (with-mocked-functions ((grpc::call-start-batch
                             (c-call ops num-ops tag)
                             (declare (ignore c-call ops num-ops tag))
                             :grpc-call-ok)
                            (grpc::completion-queue-pluck
                             (completion_queue tag)
                             (declare (ignore completion_queue tag))
                             nil)
                            (grpc::grpc-ops-clear
                             (ops size)
                             (setf ops-cleared-p t)
                             (funcall orig-ops-clear ops size)))
      (assert-false (grpc::receive-message call-object))
      (assert-true ops-cleared-p))))

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

(deftest test-slice-and-byte-buffer-with-null-bytes (server-suite)
  "Validate that convert-grpc-slice-to-bytes and get-bytes-from-grpc-byte-buffer
preserve embedded and leading null (0x00) bytes without truncating."
  (dolist (expected (list (make-array 0 :element-type '(unsigned-byte 8))
                          (make-array 4 :element-type '(unsigned-byte 8)
                                        :initial-contents '(10 0 16 3))
                          (make-array 5 :element-type '(unsigned-byte 8)
                                        :initial-contents '(0 1 0 2 0))))
    (let ((slice (grpc::convert-bytes-to-grpc-slice expected)))
      (unwind-protect
           (assert-equalp expected (grpc::convert-grpc-slice-to-bytes slice))
        (grpc::free-slice slice)))
    (let ((byte-buffer (grpc::convert-bytes-to-grpc-byte-buffer expected)))
      (unwind-protect
           (progn
             (assert-equalp expected (grpc::get-bytes-from-grpc-byte-buffer byte-buffer))
             (assert-equalp expected (grpc::get-bytes-from-grpc-byte-buffer byte-buffer 0)))
        (grpc::grpc-byte-buffer-destroy byte-buffer)))))

(deftest test-client-close-frees-ops (server-suite)
  "Validate that client-close clears close-op on both success and error paths."
  (let ((call-object (make-null-call))
        (ops-clear-count 0)
        (orig-ops-clear #'grpc::grpc-ops-clear))
    (with-mocked-functions ((grpc::call-start-batch
                             (c-call ops num-ops tag)
                             (declare (ignore c-call ops num-ops tag))
                             :grpc-call-ok)
                            (grpc::completion-queue-pluck
                             (completion_queue tag)
                             (declare (ignore completion_queue tag))
                             t)
                            (grpc::grpc-ops-clear
                             (ops size)
                             (incf ops-clear-count)
                             (funcall orig-ops-clear ops size)))
      (grpc::client-close call-object)
      (assert-eql 1 ops-clear-count))
    (with-mocked-functions ((grpc::call-start-batch
                             (c-call ops num-ops tag)
                             (declare (ignore c-call ops num-ops tag))
                             :grpc-call-error)
                            (grpc::grpc-ops-clear
                             (ops size)
                             (incf ops-clear-count)
                             (funcall orig-ops-clear ops size)))
      (assert-condition grpc:grpc-call-error
                        (grpc::client-close call-object))
      (assert-eql 2 ops-clear-count))))

(deftest test-with-insecure-channel-releases-credentials (server-suite)
  "Validate that with-insecure-channel releases the created credentials and
destroys the channel."
  (let ((fake-creds (cffi:make-pointer 1))
        (fake-channel (cffi:make-pointer 2))
        (released-creds nil)
        (destroyed-channel nil))
    (with-mocked-functions ((grpc::grpc-insecure-credentials-create
                             ()
                             fake-creds)
                            (grpc::create-channel
                             (address &optional creds args)
                             (declare (ignore address args))
                             (assert-equalp fake-creds creds)
                             fake-channel)
                            (grpc::grpc-credentials-release
                             (creds)
                             (setf released-creds creds))
                            (grpc::grpc-channel-destroy
                             (channel)
                             (setf destroyed-channel channel)))
      (grpc:with-insecure-channel (ch "localhost:50051")
        (assert-equalp fake-channel ch)))
    (assert-equalp fake-creds released-creds)
    (assert-equalp fake-channel destroyed-channel)))

(deftest test-slice-and-byte-buffer-conversions (server-suite)
  "Validate slice and byte-buffer conversion helpers round-trip bytes without
crashing or corrupting memory."
  (let* ((bytes (flexi-streams:string-to-octets "hello grpc"))
         (slice (grpc::convert-bytes-to-grpc-slice (coerce bytes 'list)))
         (roundtrip-from-slice (grpc::convert-grpc-slice-to-bytes slice))
         (buf-from-slice (grpc::convert-grpc-slice-to-grpc-byte-buffer slice))
         (buf-from-bytes (grpc::convert-bytes-to-grpc-byte-buffer bytes)))
    (grpc::free-slice slice)
    (assert-equalp bytes roundtrip-from-slice)
    (assert-eql 1 (grpc::get-grpc-byte-buffer-slice-buffer-count buf-from-slice))
    (assert-equalp bytes (grpc::get-bytes-from-grpc-byte-buffer buf-from-slice))
    (assert-equalp bytes (grpc::get-bytes-from-grpc-byte-buffer buf-from-slice 0))
    (grpc::grpc-byte-buffer-destroy buf-from-slice)
    (assert-eql 1 (grpc::get-grpc-byte-buffer-slice-buffer-count buf-from-bytes))
    (assert-equalp bytes (grpc::get-bytes-from-grpc-byte-buffer buf-from-bytes))
    (assert-equalp bytes (grpc::get-bytes-from-grpc-byte-buffer buf-from-bytes 0))
    (grpc::grpc-byte-buffer-destroy buf-from-bytes)))

(deftest test-prepare-and-free-ops-with-metadata (server-suite)
  "Validate that prepare-ops with send-metadata, send-message,
client-recv-status, and server-send-status-details can be freed cleanly by
grpc-ops-free."
  (let* ((bytes (flexi-streams:string-to-octets "payload"))
         (buf (grpc::convert-bytes-to-grpc-byte-buffer bytes))
         (ops (grpc::create-new-grpc-ops 4))
         (plist (grpc::prepare-ops ops
                                   :send-metadata '(("k1" "v1") ("k2" "v2"))
                                   :send-message buf
                                   :client-recv-status t
                                   :server-send-status :grpc-status-invalid-argument
                                   :server-send-status-details "bad argument")))
    (assert-eql 0 (getf plist :send-metadata))
    (assert-eql 1 (getf plist :send-message))
    (assert-eql 2 (getf plist :client-recv-status))
    (assert-eql 3 (getf plist :server-send-status))
    (grpc::grpc-ops-free ops 4)))

(deftest test-send-message-with-initial-metadata (server-suite)
  "Validate that send-message allocates and passes 2 ops when initial metadata
has not yet been sent on a call with context, and 1 op on subsequent sends."
  (let* ((call-object (grpc::make-call
                       :c-call (cffi:null-pointer)
                       :c-tag (cffi:null-pointer)
                       :c-ops (cffi:null-pointer)
                       :ops-plist nil
                       :context (grpc:make-context :metadata '(("x-key" "x-val")))
                       :initial-metadata-sent-p nil))
         (bytes (flexi-streams:string-to-octets "hello"))
         (batch-num-ops nil)
         (cleared-num-ops nil)
         (orig-clear-ops #'grpc::grpc-ops-clear))
    (with-mocked-functions ((grpc::call-start-batch
                             (c-call ops num-ops tag)
                             (declare (ignore c-call ops tag))
                             (push num-ops batch-num-ops)
                             :grpc-call-ok)
                            (grpc::completion-queue-pluck
                             (completion-queue tag)
                             (declare (ignore completion-queue tag))
                             t)
                            (grpc::grpc-ops-clear
                             (ops size)
                             (push size cleared-num-ops)
                             (funcall orig-clear-ops ops size)))
      (assert-true (grpc::send-message call-object bytes))
      (assert-true (grpc::call-initial-metadata-sent-p call-object))
      (assert-true (grpc::send-message call-object bytes)))
    (assert-equal '(2 1) (nreverse batch-num-ops))
    (assert-equal '(2 1) (nreverse cleared-num-ops))))

(deftest test-concatenate-byte-vectors (server-suite)
  "Validate that concatenate-byte-vectors handles empty, single, and large
numbers of slices without hitting CALL-ARGUMENTS-LIMIT."
  (assert-equalp (make-array 0 :element-type '(unsigned-byte 8))
                 (grpc::concatenate-byte-vectors nil))
  (let ((single (make-array 3 :element-type '(unsigned-byte 8)
                              :initial-contents '(1 2 3))))
    (assert-eql single (grpc::concatenate-byte-vectors (list single))))
  (let* ((count 100)
         (slices (loop for i below count
                       collect (make-array 2 :element-type '(unsigned-byte 8)
                                             :initial-contents (list i (1+ i)))))
         (result (grpc::concatenate-byte-vectors slices)))
    (assert-eql (* count 2) (length result))
    (loop for i below count
          do (assert-eql i (aref result (* i 2)))
             (assert-eql (1+ i) (aref result (1+ (* i 2)))))))

(deftest test-start-call-on-server-null-call (server-suite)
  "Validate that start-call-on-server returns nil and dispatch-requests exits
cleanly when grpc-server-request-call returns a null call pointer."
  (with-mocked-functions ((grpc::grpc-server-request-call
                           (server details metadata cq-bound cq-notify tag)
                           (declare (ignore server details metadata cq-bound cq-notify tag))
                           (cffi:null-pointer)))
    (assert-false (grpc::start-call-on-server (cffi:null-pointer)))
    (assert-false (grpc:dispatch-requests nil (cffi:null-pointer)))))

(deftest test-call-completion-queue-and-cleanup (server-suite)
  "Validate that call operations pluck the call's dedicated completion queue
and that free-call-data destroys the completion queue only when owns-cq-p is true."
  (let* ((custom-cq (cffi:make-pointer 42))
         (plucked-cqs nil)
         (destroyed-cqs nil)
         (call-object (grpc::make-call
                       :c-call (cffi:null-pointer)
                       :c-tag (cffi:null-pointer)
                       :c-ops (cffi:null-pointer)
                       :c-cq custom-cq
                       :owns-cq-p t
                       :ops-plist nil)))
    (with-mocked-functions ((grpc::call-start-batch
                             (c-call ops num-ops tag)
                             (declare (ignore c-call ops num-ops tag))
                             :grpc-call-ok)
                            (grpc::completion-queue-pluck
                             (completion-queue tag)
                             (declare (ignore tag))
                             (push completion-queue plucked-cqs)
                             t)
                            (grpc::grpc-call-unref
                             (c-call)
                             (declare (ignore c-call))
                             nil)
                            (grpc::destroy-completion-queue
                             (cq)
                             (push cq destroyed-cqs)))
      (grpc::send-message call-object (flexi-streams:string-to-octets "hi"))
      (grpc::server-send-status call-object :grpc-status-ok nil)
      (grpc::free-call-data call-object))
    (assert-eql 2 (length plucked-cqs))
    (assert-true (every (lambda (cq) (cffi:pointer-eq cq custom-cq)) plucked-cqs))
    (assert-eql 1 (length destroyed-cqs))
    (assert-true (cffi:pointer-eq custom-cq (first destroyed-cqs)))
    (assert-false (grpc::call-c-cq call-object))
    (assert-false (grpc::call-owns-cq-p call-object))))

