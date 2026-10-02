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
       (let* ((metadata (when (grpc::call-context call)
                          (grpc::context-metadata (grpc::call-context call))))
              (is-val (when metadata
                        (second (assoc "is" metadata :test #'string=)))))
         (format t "~% response: ~A ~%" message)
         (concatenate 'string
                      message
                      " Back"
                      (if is-val (format nil " ~A" is-val) ""))))))
   :dispatch-requests
   (lambda (method server)
     (bordeaux-threads:signal-semaphore sem)
     (grpc::dispatch-requests method server :exit-count 1))))

(defvar *google-inited* nil)

(defun ensure-google-init ()
  (unless *google-inited*
    ;; init
    (setf *google-inited* t)))

(deftest test-client-server-integration-success (server-suite)
  (ensure-google-init)
  (grpc:init-grpc)
  (unwind-protect
       (let* ((expected-client-response "Hello World Back Lyra")
              (hostname "localhost")
              (method-name "xyz")
              (port-number 8100)
              (sem (bordeaux-threads:make-semaphore))
              (thread (bordeaux-threads:make-thread
                       (lambda () (run-server sem hostname method-name
                                              port-number)))))
         (bordeaux-threads:wait-on-semaphore sem)
         (grpc:with-insecure-channel
             (channel
              (concatenate 'string hostname ":" (write-to-string port-number)))
           (let* ((client-context
                   (grpc::make-context :metadata '(("my" "name")
                                                   ("is" "Lyra"))))
                  (message "Hello World")
                  (response (grpc:grpc-call channel method-name
                                            (flexi-streams:string-to-octets message)
                                            client-context
                                            nil nil))
                  (actual-client-response (flexi-streams:octets-to-string
                                           (car response))))
             (assert-true (string= actual-client-response expected-client-response))
             (bordeaux-threads:join-thread thread))))
    (grpc:shutdown-grpc)))

(deftest test-client-streaming-integration (server-suite)
  (ensure-google-init)
  (grpc:init-grpc)
  (unwind-protect
       (let* ((hostname "localhost")
              (port-number 8101)
              (address (format nil "~A:~D" hostname port-number))
              (method-name "client-stream-method")
              (sem (bordeaux-threads:make-semaphore))
              (thread
                (bordeaux-threads:make-thread
                 (lambda ()
                   (grpc::run-grpc-server
                    address
                    (list
                     (grpc::make-method-details
                      :name method-name
                      :input-streaming-p t
                      :output-streaming-p nil
                      :serializer #'flexi-streams:string-to-octets
                      :deserializer #'identity
                      :action
                      (lambda (call)
                        (let (parts)
                          (loop for msg = (grpc::receive-message call)
                                while msg
                                do (push (flexi-streams:octets-to-string
                                          (grpc::concatenate-byte-vectors msg)
                                          :external-format :utf-8)
                                         parts))
                          (format nil "~{~A~^, ~}" (nreverse parts))))))
                    :dispatch-requests
                    (lambda (methods server)
                      (bordeaux-threads:signal-semaphore sem)
                      (grpc::dispatch-requests methods server :exit-count 1)))))))
         (bordeaux-threads:wait-on-semaphore sem)
         (grpc:with-insecure-channel (channel address)
           (let* ((requests (mapcar #'flexi-streams:string-to-octets
                                    '("Alpha" "Beta" "Gamma")))
                  (response (grpc:grpc-call channel method-name requests nil nil t))
                  (actual (flexi-streams:octets-to-string
                           (grpc::concatenate-byte-vectors response)
                           :external-format :utf-8)))
             (assert-equal "Alpha, Beta, Gamma" actual)))
         (bordeaux-threads:join-thread thread))
    (grpc:shutdown-grpc)))

(deftest test-server-streaming-integration (server-suite)
  (ensure-google-init)
  (grpc:init-grpc)
  (unwind-protect
       (let* ((hostname "localhost")
              (port-number 8102)
              (address (format nil "~A:~D" hostname port-number))
              (method-name "server-stream-method")
              (sem (bordeaux-threads:make-semaphore))
              (thread
                (bordeaux-threads:make-thread
                 (lambda ()
                   (grpc::run-grpc-server
                    address
                    (list
                     (grpc::make-method-details
                      :name method-name
                      :input-streaming-p nil
                      :output-streaming-p t
                      :serializer #'identity
                      :deserializer
                      (lambda (bytes)
                        (flexi-streams:octets-to-string bytes :external-format :utf-8))
                      :action
                      (lambda (message call)
                        (dotimes (i 3)
                          (grpc::send-message
                           call
                           (flexi-streams:string-to-octets
                            (format nil "~A ~D" message i)))))))
                    :dispatch-requests
                    (lambda (methods server)
                      (bordeaux-threads:signal-semaphore sem)
                      (grpc::dispatch-requests methods server :exit-count 1)))))))
         (bordeaux-threads:wait-on-semaphore sem)
         (grpc:with-insecure-channel (channel address)
           (let* ((responses (grpc:grpc-call channel method-name
                                             (flexi-streams:string-to-octets "Item")
                                             nil t nil))
                  (actual (mapcar (lambda (msg)
                                    (flexi-streams:octets-to-string
                                     (grpc::concatenate-byte-vectors msg)
                                     :external-format :utf-8))
                                  responses)))
             (assert-equal '("Item 0" "Item 1" "Item 2") actual)))
         (bordeaux-threads:join-thread thread))
    (grpc:shutdown-grpc)))

(deftest test-bidirectional-streaming-integration (server-suite)
  (ensure-google-init)
  (grpc:init-grpc)
  (unwind-protect
       (let* ((hostname "localhost")
              (port-number 8103)
              (address (format nil "~A:~D" hostname port-number))
              (method-name "bidi-stream-method")
              (sem (bordeaux-threads:make-semaphore))
              (thread
                (bordeaux-threads:make-thread
                 (lambda ()
                   (grpc::run-grpc-server
                    address
                    (list
                     (grpc::make-method-details
                      :name method-name
                      :input-streaming-p t
                      :output-streaming-p t
                      :serializer #'identity
                      :deserializer #'identity
                      :action
                      (lambda (call)
                        (loop for msg = (grpc::receive-message call)
                              while msg
                              do (let ((text (flexi-streams:octets-to-string
                                              (grpc::concatenate-byte-vectors msg)
                                              :external-format :utf-8)))
                                   (dotimes (i 2)
                                     (grpc::send-message
                                      call
                                      (flexi-streams:string-to-octets
                                       (format nil "~A-~D" text i)))))))))
                    :dispatch-requests
                    (lambda (methods server)
                      (bordeaux-threads:signal-semaphore sem)
                      (grpc::dispatch-requests methods server :exit-count 1)))))))
         (bordeaux-threads:wait-on-semaphore sem)
         (grpc:with-insecure-channel (channel address)
           (let* ((requests (mapcar #'flexi-streams:string-to-octets '("ReqA" "ReqB")))
                  (responses (grpc:grpc-call channel method-name requests nil t t))
                  (actual (mapcar (lambda (msg)
                                    (flexi-streams:octets-to-string
                                     (grpc::concatenate-byte-vectors msg)
                                     :external-format :utf-8))
                                  responses)))
             (assert-equal '("ReqA-0" "ReqA-1" "ReqB-0" "ReqB-1") actual)))
         (bordeaux-threads:join-thread thread))
    (grpc:shutdown-grpc)))

(deftest test-binary-payload-with-null-bytes-integration (server-suite)
  (ensure-google-init)
  (grpc:init-grpc)
  (unwind-protect
       (let* ((hostname "localhost")
              (port-number 8104)
              (address (format nil "~A:~D" hostname port-number))
              (method-name "binary-echo")
              (payload (make-array 8 :element-type '(unsigned-byte 8)
                                     :initial-contents '(0 1 0 2 128 0 255 0)))
              (trailer (make-array 3 :element-type '(unsigned-byte 8)
                                     :initial-contents '(0 42 0)))
              (expected (grpc::concatenate-byte-vectors (list payload trailer)))
              (sem (bordeaux-threads:make-semaphore))
              (thread
                (bordeaux-threads:make-thread
                 (lambda ()
                   (grpc::run-grpc-server
                    address
                    (list
                     (grpc::make-method-details
                      :name method-name
                      :serializer #'identity
                      :deserializer #'identity
                      :action
                      (lambda (message call)
                        (declare (ignore call))
                        (grpc::concatenate-byte-vectors (list message trailer)))))
                    :dispatch-requests
                    (lambda (methods server)
                      (bordeaux-threads:signal-semaphore sem)
                      (grpc::dispatch-requests methods server :exit-count 1)))))))
         (bordeaux-threads:wait-on-semaphore sem)
         (grpc:with-insecure-channel (channel address)
           (let* ((response (grpc:grpc-call channel method-name payload nil nil nil))
                  (actual (grpc::concatenate-byte-vectors response)))
             (assert-equalp expected actual)))
         (bordeaux-threads:join-thread thread))
    (grpc:shutdown-grpc)))

(deftest test-non-ok-status-codes-integration (server-suite)
  (ensure-google-init)
  (grpc:init-grpc)
  (unwind-protect
       (let* ((hostname "localhost")
              (port-number 8105)
              (address (format nil "~A:~D" hostname port-number))
              (method-name "abort-method")
              (sem (bordeaux-threads:make-semaphore))
              (thread
                (bordeaux-threads:make-thread
                 (lambda ()
                   (grpc::run-grpc-server
                    address
                    (list
                     (grpc::make-method-details
                      :name method-name
                      :serializer #'flexi-streams:string-to-octets
                      :deserializer
                      (lambda (bytes)
                        (flexi-streams:octets-to-string bytes :external-format :utf-8))
                      :action
                      (lambda (message call)
                        (declare (ignore call))
                        (when (string= message "abort")
                          (grpc:abort-server-stream :grpc-status-invalid-argument
                                                    "Invalid argument"))
                        message)))
                    :dispatch-requests
                    (lambda (methods server)
                      (bordeaux-threads:signal-semaphore sem)
                      (grpc::dispatch-requests methods server :exit-count 2)))))))
         (bordeaux-threads:wait-on-semaphore sem)
         (grpc:with-insecure-channel (channel address)
           (let ((err-status nil))
             (handler-case
                 (grpc:grpc-call channel method-name
                                 (flexi-streams:string-to-octets "abort")
                                 nil nil nil)
               (grpc::grpc-call-error (c)
                 (setf err-status (grpc::call-error c))))
             (assert-eql :grpc-status-invalid-argument err-status))
           (let ((unimpl-status nil))
             (handler-case
                 (grpc:grpc-call channel "unregistered-method"
                                 (flexi-streams:string-to-octets "hello")
                                 nil nil nil)
               (grpc::grpc-call-error (c)
                 (setf unimpl-status (grpc::call-error c))))
             (assert-eql :grpc-status-unimplemented unimpl-status)))
         (bordeaux-threads:join-thread thread))
    (grpc:shutdown-grpc)))

(deftest test-async-grpc-call-integration (server-suite)
  (ensure-google-init)
  (grpc:init-grpc)
  (unwind-protect
       (let* ((expected-client-response "Hello Async Back Lyra")
              (hostname "localhost")
              (method-name "async-xyz")
              (port-number 8106)
              (sem (bordeaux-threads:make-semaphore))
              (thread (bordeaux-threads:make-thread
                       (lambda () (run-server sem hostname method-name
                                              port-number)))))
         (bordeaux-threads:wait-on-semaphore sem)
         (grpc:with-insecure-channel
             (channel
              (concatenate 'string hostname ":" (write-to-string port-number)))
           (let* ((client-context
                    (grpc::make-context :metadata '(("my" "name")
                                                    ("is" "Lyra"))))
                  (callback-result nil)
                  (async-call
                    (grpc:grpc-call channel method-name
                                    (flexi-streams:string-to-octets "Hello Async")
                                    client-context
                                    nil nil
                                    :callback (lambda (resp)
                                                (setf callback-result
                                                      (flexi-streams:octets-to-string
                                                       (car resp))))))
                  (waited-response (grpc:async-call-wait async-call))
                  (actual-client-response (flexi-streams:octets-to-string
                                           (car waited-response))))
             (assert-true (grpc:async-call-p async-call))
             (assert-true (grpc:async-call-ready-p async-call))
             (assert-equal expected-client-response callback-result)
             (assert-equal expected-client-response actual-client-response)
             (bordeaux-threads:join-thread thread))))
    (grpc:shutdown-grpc)))

