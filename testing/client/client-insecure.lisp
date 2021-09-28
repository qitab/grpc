;; Copyright 2016-2021 Google LLC
;;
;; Use of this source code is governed by an MIT-style
;; license that can be found in the LICENSE file or at
;; https://opensource.org/licenses/MIT.

;;  A simple test for gRPC Client in Common Lisp using an insecure channel.

(defpackage #:testing-client-insecure
  (:use #:common-lisp)
  (:local-nicknames (#:testing #:cl-protobufs)
                    (#:testing-grpc #:cl-protobufs)
                    (#:flag #:ace.flag))
  (:export #:main))

(in-package #:testing-client-insecure)

(flag:define hostname "localhost"
  "Name of server that will be connected to."
  :names ("hostname")
  :type STRING)

(flag:define port-number 8080
  "Port of server to establish connection to."
  :names ("port")
  :type INTEGER)

(defun main ()
  (grpc:init-grpc)
  (unwind-protect
       (grpc:with-insecure-channel
           (channel (concatenate 'string hostname ":" (write-to-string port-number)))

         ;; Unary streaming
         (let* ((message (cl-protobufs.testing:make-hello-request :name "Neo"))
                (response (cl-protobufs.testing-rpc:call-say-hello channel message)))
           (format nil "Response: ~A" (cl-protobufs.testing:hello-reply.message response))
           response)

         ;; Server Streaming
         (let* ((message (cl-protobufs.testing:make-hello-request
                          :name "Neo" :num-responses 3))
                (response (cl-protobufs.testing-rpc:call-say-hello-server-stream channel message)))
           (loop for message in response
                 do
              (format nil "Response: ~A" (cl-protobufs.testing:hello-reply.message message))))

         ;; Client Streaming
         (let* ((messages (list (cl-protobufs.testing:make-hello-request :name "Pika")
                                (cl-protobufs.testing:make-hello-request :name "Chu")
                                (cl-protobufs.testing:make-hello-request :name "Char")
                                (cl-protobufs.testing:make-hello-request :name "Mander")))
                (response (cl-protobufs.testing-rpc:call-say-hello-client-stream channel messages)))
           (format nil "Response: ~A" (cl-protobufs.testing:hello-reply.message response)))

         ;; Bidirectional Streaming.
         (let* ((messages (list (cl-protobufs.testing:make-hello-request
                                 :name "Pika" :num-responses 1)
                                (cl-protobufs.testing:make-hello-request
                                 :name "Chu" :num-responses 2)
                                (cl-protobufs.testing:make-hello-request
                                 :name "Char" :num-responses 3)))
                (response (cl-protobufs.testing-rpc:call-say-hello-bidirectional-stream channel messages)))
           (loop for message in response
                 do
              (format nil "Response: ~A" (cl-protobufs.testing:hello-reply.message message)))))
    (grpc:shutdown-grpc)))
