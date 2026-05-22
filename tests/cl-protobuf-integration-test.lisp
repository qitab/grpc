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
  (when (string= (ut:hello-request.name request) "prolonged")
    (sleep 1))
  (let* ((metadata (when (grpc::call-context rpc)
                     (grpc::context-metadata (grpc::call-context rpc))))
         (is-val (when metadata
                   (second (assoc "is" metadata :test #'string=)))))
    (ut:make-hello-reply
     :message
     (concatenate 'string
                  (ut:hello-request.name request)
                  " Back"
                  (if is-val (format nil " ~A" is-val) "")))))


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

(defvar *google-inited* nil)

(deftest test-client-server-integration-success (proto-server-suite)
  (unless *google-inited*
    ;; init
    (setf *google-inited* t))
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

(deftest test-client-server-integration-timeout-success (proto-server-suite)
  (unless *google-inited*
    ;; init
    (setf *google-inited* t))
  (grpc:init-grpc)
  (unwind-protect
       (let* ((expected-client-response "Hello World Back")
              (hostname "localhost")
              (port-number 8001)
              (sem (bordeaux-threads:make-semaphore))
              (thread (bordeaux-threads:make-thread
                       (lambda () (run-server sem hostname port-number)))))

         (bordeaux-threads:wait-on-semaphore sem)

         (grpc:with-insecure-channel
             (channel (concatenate 'string hostname ":"
                                   (write-to-string port-number)))
           ;; Unary streaming with a generous timeout
           (let* ((message (ut:make-hello-request :name "Hello World"))
                  (response (ut-rpc:call-say-hello channel message :timeout 5.0d0)))
             (assert-true (string= (ut:hello-reply.message response)
                                   expected-client-response))))
         (bordeaux-threads:join-thread thread))
    (grpc:shutdown-grpc)))

(deftest test-client-server-integration-timeout-exceeded (proto-server-suite)
  (unless *google-inited*
    ;; init
    (setf *google-inited* t))
  (grpc:init-grpc)
  (unwind-protect
       (let* ((hostname "localhost")
              (port-number 8002)
              (sem (bordeaux-threads:make-semaphore))
              (thread (bordeaux-threads:make-thread
                       (lambda () (run-server sem hostname port-number)))))

         (bordeaux-threads:wait-on-semaphore sem)

         (grpc:with-insecure-channel
             (channel (concatenate 'string hostname ":"
                                   (write-to-string port-number)))
           ;; Unary streaming with a short timeout that will be exceeded
           (let* ((message (ut:make-hello-request :name "prolonged")))
             (assert-condition grpc::grpc-call-error
                               (ut-rpc:call-say-hello channel message :timeout 0.1d0))))
         (bordeaux-threads:join-thread thread))
    (grpc:shutdown-grpc)))

;; Streaming Server implementations

(defmethod ut-rpc::say-hello-server-stream ((request ut:hello-request) call)
  (dotimes (i (ut:hello-request.num-responses request))
    (grpc:stream-send call (ut:make-hello-reply :message (format nil "Reply ~D to ~A" i (ut:hello-request.name request))))))

(defmethod ut-rpc::say-hello-client-stream (call)
  (let (names)
    (grpc:do-stream-receive (req call)
      (push (ut:hello-request.name req) names))
    (ut:make-hello-reply :message (format nil "~{~A~^, ~}" (nreverse names)))))

(defmethod ut-rpc::say-hello-bidirectional-stream (call)
  (grpc:do-stream-receive (req call)
    (when (string= (ut:hello-request.name req) "abort")
      (grpc:abort-server-stream :grpc-status-invalid-argument "Stream aborted by client request"))
    (dotimes (i (ut:hello-request.num-responses req))
      (grpc:stream-send call (ut:make-hello-reply :message (format nil "Bidi ~D to ~A" i (ut:hello-request.name req)))))))

;; Streaming Unit Tests

(deftest test-server-streaming (proto-server-suite)
  (unless *google-inited*
    ;; init
    (setf *google-inited* t))
  (grpc:init-grpc)
  (unwind-protect
       (let* ((hostname "localhost")
              (port-number 8003)
              (sem (bordeaux-threads:make-semaphore))
              (thread (bordeaux-threads:make-thread
                       (lambda () (run-server sem hostname port-number)))))
         (bordeaux-threads:wait-on-semaphore sem)
         (grpc:with-insecure-channel (channel (concatenate 'string hostname ":" (write-to-string port-number)))
           (grpc:with-client-stream (call (ut-rpc:say-hello-server-stream/start channel))
             (grpc:stream-send call (ut:make-hello-request :name "Alice" :num-responses 3))
             (let ((replies (loop for rep = (grpc:stream-receive call)
                                  while rep
                                  collect (ut:hello-reply.message rep))))
               (assert-true (equal replies '("Reply 0 to Alice" "Reply 1 to Alice" "Reply 2 to Alice"))))))
         (bordeaux-threads:join-thread thread))
    (grpc:shutdown-grpc)))

(deftest test-client-streaming (proto-server-suite)
  (unless *google-inited*
    ;; init
    (setf *google-inited* t))
  (grpc:init-grpc)
  (unwind-protect
       (let* ((hostname "localhost")
              (port-number 8004)
              (sem (bordeaux-threads:make-semaphore))
              (thread (bordeaux-threads:make-thread
                       (lambda () (run-server sem hostname port-number)))))
         (bordeaux-threads:wait-on-semaphore sem)
         (grpc:with-insecure-channel (channel (concatenate 'string hostname ":" (write-to-string port-number)))
           (grpc:with-client-stream (call (ut-rpc:say-hello-client-stream/start channel))
             (grpc:stream-send call (ut:make-hello-request :name "Bob"))
             (grpc:stream-send call (ut:make-hello-request :name "Charlie"))
             (grpc:stream-close call)
             (let ((reply (grpc:stream-receive call)))
               (assert-true (string= (ut:hello-reply.message reply) "Bob, Charlie")))))
         (bordeaux-threads:join-thread thread))
    (grpc:shutdown-grpc)))

(deftest test-bidirectional-streaming (proto-server-suite)
  (unless *google-inited*
    ;; init
    (setf *google-inited* t))
  (grpc:init-grpc)
  (unwind-protect
       (let* ((hostname "localhost")
              (port-number 8005)
              (sem (bordeaux-threads:make-semaphore))
              (thread (bordeaux-threads:make-thread
                       (lambda () (run-server sem hostname port-number)))))
         (bordeaux-threads:wait-on-semaphore sem)
         (grpc:with-insecure-channel (channel (concatenate 'string hostname ":" (write-to-string port-number)))
           (grpc:with-client-stream (call (ut-rpc:say-hello-bidirectional-stream/start channel))
             (grpc:stream-send call (ut:make-hello-request :name "Dave" :num-responses 2))
             (grpc:stream-send call (ut:make-hello-request :name "Eve" :num-responses 1))
             (grpc:stream-close call)
             (sleep 0.5)
             (let ((replies (loop for rep = (grpc:stream-receive call)
                                  while rep
                                  collect (ut:hello-reply.message rep))))
               (assert-true (equal replies '("Bidi 0 to Dave" "Bidi 1 to Dave" "Bidi 0 to Eve"))))))
         (bordeaux-threads:join-thread thread))
    (grpc:shutdown-grpc)))

(deftest test-server-abort-streaming (proto-server-suite)
  (unless *google-inited*
    ;; init
    (setf *google-inited* t))
  (grpc:init-grpc)
  (unwind-protect
       (let* ((hostname "localhost")
              (port-number 8006)
              (sem (bordeaux-threads:make-semaphore))
              (thread (bordeaux-threads:make-thread
                       (lambda () (run-server sem hostname port-number)))))
         (bordeaux-threads:wait-on-semaphore sem)
         (grpc:with-insecure-channel (channel (concatenate 'string hostname ":" (write-to-string port-number)))
           (let ((call (ut-rpc:say-hello-bidirectional-stream/start channel)))
             (grpc:stream-send call (ut:make-hello-request :name "abort" :num-responses 1))
             (grpc:stream-close call)
             (sleep 0.5)
             (assert-condition grpc::grpc-call-error (grpc:stream-receive call))
             (grpc:stream-cleanup call)))
         (bordeaux-threads:join-thread thread))
    (grpc:shutdown-grpc)))

(deftest test-client-server-integration-metadata-success (proto-server-suite)
  (unless *google-inited*
    ;; init
    (setf *google-inited* t))
  (grpc:init-grpc)
  (unwind-protect
       (let* ((expected-client-response "Hello World Back Lyra")
              (hostname "localhost")
              (port-number 8007)
              (sem (bordeaux-threads:make-semaphore))
              (thread (bordeaux-threads:make-thread
                       (lambda () (run-server sem hostname port-number)))))

         (bordeaux-threads:wait-on-semaphore sem)

         (grpc:with-insecure-channel
             (channel (concatenate 'string hostname ":"
                                   (write-to-string port-number)))
           ;; Unary call with metadata
           (let* ((message (ut:make-hello-request :name "Hello World"))
                  (response (ut-rpc:call-say-hello channel message :metadata '(("my" "name") ("is" "Lyra")))))
             (assert-true (string= (ut:hello-reply.message response)
                                   expected-client-response))))
         (bordeaux-threads:join-thread thread))
    (grpc:shutdown-grpc)))

