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
  (when (string= (ut:hello-request.name request) "abort")
    (grpc:abort-server-stream :grpc-status-invalid-argument
                              "Unary call aborted by client request"))
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


(defun run-server (sem hostname port-number &key (exit-count 1))
  (grpc::run-grpc-proto-server
   (concatenate 'string
                hostname ":"
                (write-to-string port-number))
   'ut:greeter
   :dispatch-requests
   (lambda (method server)
     (bordeaux-threads:signal-semaphore sem)
     (grpc::dispatch-requests method server :exit-count exit-count))))

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
  (when (string= (ut:hello-request.name request) "abort")
    (grpc:abort-server-stream :grpc-status-permission-denied
                              "Server stream aborted by client request"))
  (dotimes (i (ut:hello-request.num-responses request))
    (grpc:stream-send call (ut:make-hello-reply :message (format nil "Reply ~D to ~A" i (ut:hello-request.name request))))))

(defmethod ut-rpc::say-hello-client-stream (call)
  (let (names)
    (grpc:do-stream-receive (req call)
      (when (string= (ut:hello-request.name req) "abort")
        (grpc:abort-server-stream :grpc-status-invalid-argument
                                  "Client stream aborted by client request"))
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

(deftest test-client-server-integration-null-bytes-success (proto-server-suite)
  (unless *google-inited*
    ;; init
    (setf *google-inited* t))
  (grpc:init-grpc)
  (unwind-protect
       (let* ((null-name (format nil "Hello~CWorld" #\Null))
              (expected-client-response (concatenate 'string null-name " Back"))
              (hostname "localhost")
              (port-number 8008)
              (sem (bordeaux-threads:make-semaphore))
              (thread (bordeaux-threads:make-thread
                       (lambda () (run-server sem hostname port-number)))))

         (bordeaux-threads:wait-on-semaphore sem)

         (grpc:with-insecure-channel
             (channel (concatenate 'string hostname ":"
                                   (write-to-string port-number)))
           (let* ((message (ut:make-hello-request :name null-name))
                  (response (ut-rpc:call-say-hello channel message)))
             (assert-true (string= (ut:hello-reply.message response)
                                   expected-client-response))))
         (bordeaux-threads:join-thread thread))
    (grpc:shutdown-grpc)))

(deftest test-concurrent-calls-exceeding-pluck-limit (proto-server-suite)
  "Verify that more than GRPC_MAX_COMPLETION_QUEUE_PLUCKERS (6) concurrent
server worker threads and concurrent client RPCs succeed."
  (unless *google-inited*
    ;; init
    (setf *google-inited* t))
  (grpc:init-grpc)
  (unwind-protect
       (let* ((num-concurrent 10)
              (hostname "localhost")
              (port-number 8009)
              (address (format nil "~A:~D" hostname port-number))
              (ready-sem (bordeaux-threads:make-semaphore))
              (server-thread
                (bordeaux-threads:make-thread
                 (lambda ()
                   (grpc::run-grpc-proto-server
                    address
                    'ut:greeter
                    :num-threads num-concurrent
                    :dispatch-requests
                    (lambda (methods server)
                      (bordeaux-threads:signal-semaphore ready-sem)
                      (grpc::dispatch-requests methods server :exit-count 1)))))))
         (dotimes (i num-concurrent)
           (bordeaux-threads:wait-on-semaphore ready-sem))
         ;; Give all 10 server threads a moment to enter grpc_completion_queue_pluck.
         (sleep 0.1)
         (grpc:with-insecure-channel (channel address)
           (let* ((results (make-array num-concurrent :initial-element nil))
                  (client-threads
                    (loop for i below num-concurrent
                          collect (let ((idx i))
                                    (bordeaux-threads:make-thread
                                     (lambda ()
                                       (let* ((req (ut:make-hello-request :name "prolonged"))
                                              (resp (ut-rpc:call-say-hello channel req)))
                                         (setf (aref results idx)
                                               (and resp (ut:hello-reply.message resp))))))))))
             (dolist (ct client-threads)
               (bordeaux-threads:join-thread ct))
             (dotimes (i num-concurrent)
               (assert-equal "prolonged Back" (aref results i)))))
         (bordeaux-threads:join-thread server-thread))
    (grpc:shutdown-grpc)))

(deftest test-one-shot-server-streaming (proto-server-suite)
  "Verify one-shot server-streaming via ut-rpc:call-say-hello-server-stream
(grpc::start-call with server-stream = t) against a live server."
  (unless *google-inited*
    ;; init
    (setf *google-inited* t))
  (grpc:init-grpc)
  (unwind-protect
       (let* ((hostname "localhost")
              (port-number 8010)
              (address (format nil "~A:~D" hostname port-number))
              (sem (bordeaux-threads:make-semaphore))
              (thread (bordeaux-threads:make-thread
                       (lambda () (run-server sem hostname port-number)))))
         (bordeaux-threads:wait-on-semaphore sem)
         (grpc:with-insecure-channel (channel address)
           (let* ((request (ut:make-hello-request :name "Neo" :num-responses 3))
                  (responses (ut-rpc:call-say-hello-server-stream channel request)))
             (assert-equal '("Reply 0 to Neo" "Reply 1 to Neo" "Reply 2 to Neo")
                           (mapcar #'ut:hello-reply.message responses))))
         (bordeaux-threads:join-thread thread))
    (grpc:shutdown-grpc)))

(deftest test-one-shot-client-streaming (proto-server-suite)
  "Verify one-shot client-streaming via ut-rpc:call-say-hello-client-stream
(grpc::start-call with client-stream = t) against a live server."
  (unless *google-inited*
    ;; init
    (setf *google-inited* t))
  (grpc:init-grpc)
  (unwind-protect
       (let* ((hostname "localhost")
              (port-number 8011)
              (address (format nil "~A:~D" hostname port-number))
              (sem (bordeaux-threads:make-semaphore))
              (thread (bordeaux-threads:make-thread
                       (lambda () (run-server sem hostname port-number)))))
         (bordeaux-threads:wait-on-semaphore sem)
         (grpc:with-insecure-channel (channel address)
           (let* ((requests (list (ut:make-hello-request :name "Neo")
                                  (ut:make-hello-request :name "Morpheus")
                                  (ut:make-hello-request :name "Trinity")))
                  (response (ut-rpc:call-say-hello-client-stream channel requests)))
             (assert-equal "Neo, Morpheus, Trinity"
                           (ut:hello-reply.message response))))
         (bordeaux-threads:join-thread thread))
    (grpc:shutdown-grpc)))

(deftest test-one-shot-bidirectional-streaming (proto-server-suite)
  "Verify one-shot bidirectional-streaming via
ut-rpc:call-say-hello-bidirectional-stream (grpc::start-call with
server-stream = t and client-stream = t) against a live server."
  (unless *google-inited*
    ;; init
    (setf *google-inited* t))
  (grpc:init-grpc)
  (unwind-protect
       (let* ((hostname "localhost")
              (port-number 8012)
              (address (format nil "~A:~D" hostname port-number))
              (sem (bordeaux-threads:make-semaphore))
              (thread (bordeaux-threads:make-thread
                       (lambda () (run-server sem hostname port-number)))))
         (bordeaux-threads:wait-on-semaphore sem)
         (grpc:with-insecure-channel (channel address)
           (let* ((requests (list (ut:make-hello-request :name "Neo" :num-responses 2)
                                  (ut:make-hello-request :name "Trinity" :num-responses 1)))
                  (responses (ut-rpc:call-say-hello-bidirectional-stream channel requests)))
             (assert-equal '("Bidi 0 to Neo" "Bidi 1 to Neo" "Bidi 0 to Trinity")
                           (mapcar #'ut:hello-reply.message responses))))
         (bordeaux-threads:join-thread thread))
    (grpc:shutdown-grpc)))

(deftest test-client-do-stream-receive (proto-server-suite)
  "Verify grpc:do-stream-receive on client-side streaming calls (both
server-streaming and bidirectional-streaming) and generated stub helpers."
  (unless *google-inited*
    ;; init
    (setf *google-inited* t))
  (grpc:init-grpc)
  (unwind-protect
       (let* ((hostname "localhost")
              (port-number 8013)
              (address (format nil "~A:~D" hostname port-number))
              (sem (bordeaux-threads:make-semaphore))
              (thread (bordeaux-threads:make-thread
                       (lambda () (run-server sem hostname port-number :exit-count 2)))))
         (bordeaux-threads:wait-on-semaphore sem)
         (grpc:with-insecure-channel (channel address)
           ;; 1. Server-streaming using grpc:do-stream-receive on the client call
           (grpc:with-client-stream (call (ut-rpc:say-hello-server-stream/start channel))
             (grpc:stream-send call (ut:make-hello-request :name "StreamUser" :num-responses 3))
             (grpc:stream-close call)
             (let (replies)
               (grpc:do-stream-receive (rep call)
                 (push (ut:hello-reply.message rep) replies))
               (assert-equal '("Reply 0 to StreamUser"
                               "Reply 1 to StreamUser"
                               "Reply 2 to StreamUser")
                             (nreverse replies))))
           ;; 2. Bidirectional-streaming using generated /start, /send, /close,
           ;;    grpc:do-stream-receive, and /cleanup helpers
           (let ((call (ut-rpc:say-hello-bidirectional-stream/start channel)))
             (ut-rpc:say-hello-bidirectional-stream/send
              call (ut:make-hello-request :name "First" :num-responses 2))
             (ut-rpc:say-hello-bidirectional-stream/send
              call (ut:make-hello-request :name "Second" :num-responses 1))
             (ut-rpc:say-hello-bidirectional-stream/close call)
             (let (replies)
               (grpc:do-stream-receive (rep call)
                 (push (ut:hello-reply.message rep) replies))
               (assert-equal '("Bidi 0 to First" "Bidi 1 to First" "Bidi 0 to Second")
                             (nreverse replies)))
             (ut-rpc:say-hello-bidirectional-stream/cleanup call)))
         (bordeaux-threads:join-thread thread))
    (grpc:shutdown-grpc)))

(deftest test-streaming-binary-null-bytes-payloads (proto-server-suite)
  "Verify that binary protobuf payloads containing 0x00 bytes (leading,
embedded, and trailing null bytes, as well as zero varints) are preserved across
client-streaming, server-streaming, and bidirectional-streaming RPCs."
  (unless *google-inited*
    ;; init
    (setf *google-inited* t))
  (grpc:init-grpc)
  (unwind-protect
       (let* ((null-str-1 (format nil "~CHead~CMid~CTail~C" #\Null #\Null #\Null #\Null))
              (null-str-2 (format nil "A~C~CB" #\Null #\Null))
              (hostname "localhost")
              (port-number 8014)
              (address (format nil "~A:~D" hostname port-number))
              (sem (bordeaux-threads:make-semaphore))
              (thread (bordeaux-threads:make-thread
                       (lambda () (run-server sem hostname port-number :exit-count 3)))))
         (bordeaux-threads:wait-on-semaphore sem)
         (grpc:with-insecure-channel (channel address)
           ;; Ensure the serialized wire payload actually contains 0x00 bytes
           (let ((req-with-nulls (ut:make-hello-request :name null-str-1 :num-responses 0)))
             (assert-true (find 0 (cl-protobufs:serialize-to-bytes req-with-nulls))))
           ;; 1. Client-streaming with 0x00 bytes
           (let* ((requests (list (ut:make-hello-request :name null-str-1 :num-responses 0)
                                  (ut:make-hello-request :name null-str-2 :num-responses 0)))
                  (reply (ut-rpc:call-say-hello-client-stream channel requests)))
             (assert-equal (format nil "~A, ~A" null-str-1 null-str-2)
                           (ut:hello-reply.message reply)))
           ;; 2. Server-streaming with 0x00 bytes
           (let* ((req (ut:make-hello-request :name null-str-1 :num-responses 2))
                  (replies (ut-rpc:call-say-hello-server-stream channel req)))
             (assert-equal (list (format nil "Reply 0 to ~A" null-str-1)
                                 (format nil "Reply 1 to ~A" null-str-1))
                           (mapcar #'ut:hello-reply.message replies)))
           ;; 3. Bidirectional-streaming with 0x00 bytes and do-stream-receive
           (grpc:with-client-stream (call (ut-rpc:say-hello-bidirectional-stream/start channel))
             (grpc:stream-send call (ut:make-hello-request :name null-str-1 :num-responses 1))
             (grpc:stream-send call (ut:make-hello-request :name null-str-2 :num-responses 1))
             (grpc:stream-close call)
             (let (replies)
               (grpc:do-stream-receive (rep call)
                 (push (ut:hello-reply.message rep) replies))
               (assert-equal (list (format nil "Bidi 0 to ~A" null-str-1)
                                   (format nil "Bidi 0 to ~A" null-str-2))
                             (nreverse replies)))))
         (bordeaux-threads:join-thread thread))
    (grpc:shutdown-grpc)))

(deftest test-non-ok-status-codes (proto-server-suite)
  "Verify that non-OK gRPC status codes are propagated and reported on
grpc::grpc-call-error across unary, client-streaming, server-streaming, and
bidirectional-streaming RPCs."
  (unless *google-inited*
    ;; init
    (setf *google-inited* t))
  (grpc:init-grpc)
  (unwind-protect
       (let* ((hostname "localhost")
              (port-number 8015)
              (address (format nil "~A:~D" hostname port-number))
              (sem (bordeaux-threads:make-semaphore))
              (thread (bordeaux-threads:make-thread
                       (lambda () (run-server sem hostname port-number :exit-count 4)))))
         (bordeaux-threads:wait-on-semaphore sem)
         (grpc:with-insecure-channel (channel address)
           ;; 1. Unary non-OK status (:grpc-status-invalid-argument)
           (let ((status nil))
             (handler-case
                 (ut-rpc:call-say-hello channel (ut:make-hello-request :name "abort"))
               (grpc::grpc-call-error (c)
                 (setf status (grpc::call-error c))))
             (assert-eql :grpc-status-invalid-argument status))
           ;; 2. Client-streaming non-OK status (:grpc-status-invalid-argument)
           (let ((status nil))
             (handler-case
                 (ut-rpc:call-say-hello-client-stream
                  channel
                  (list (ut:make-hello-request :name "Alice")
                        (ut:make-hello-request :name "abort")))
               (grpc::grpc-call-error (c)
                 (setf status (grpc::call-error c))))
             (assert-eql :grpc-status-invalid-argument status))
           ;; 3. Server-streaming non-OK status (:grpc-status-permission-denied)
           (let ((status nil))
             (handler-case
                 (ut-rpc:call-say-hello-server-stream
                  channel
                  (ut:make-hello-request :name "abort" :num-responses 2))
               (grpc::grpc-call-error (c)
                 (setf status (grpc::call-error c))))
             (assert-eql :grpc-status-permission-denied status))
           ;; 4. Bidirectional-streaming non-OK status with do-stream-receive
           (let ((status nil)
                 (received nil))
             (handler-case
                 (grpc:with-client-stream
                     (call (ut-rpc:say-hello-bidirectional-stream/start channel))
                   (grpc:stream-send call (ut:make-hello-request :name "Ok" :num-responses 1))
                   (grpc:stream-send call (ut:make-hello-request :name "abort" :num-responses 1))
                   (grpc:stream-close call)
                   (grpc:do-stream-receive (rep call)
                     (push (ut:hello-reply.message rep) received)))
               (grpc::grpc-call-error (c)
                 (setf status (grpc::call-error c))))
             (assert-equal '("Bidi 0 to Ok") (nreverse received))
             (assert-eql :grpc-status-invalid-argument status)))
         (bordeaux-threads:join-thread thread))
    (grpc:shutdown-grpc)))


