;; Copyright 2016-2020 Google LLC
;;
;; Use of this source code is governed by an MIT-style
;; license that can be found in the LICENSE file or at
;; https://opensource.org/licenses/MIT.

;;  A simple test for gRPC Client in Common Lisp

(defpackage #:testing-client
  (:use #:common-lisp)
  (:local-nicknames (#:testing #:cl-protobufs)
                   (#:testing-grpc #:cl-protobufs)
                   (#:log #:google.log)
                   (#:flag #:ace.flag))
  (:export #:main))

(in-package #:testing-client)

(flag:define hostname "localhost"
  "Name of server that will be connected to."
  :names ("hostname")
  :type STRING)

(flag:define port-number 8080
  "Port of server to establish connection to."
  :names ("port")
  :type INTEGER)

(defun main ()
  (google:init)
  (grpc:init-grpc)
  (grpc:with-loas2-channel
      (channel
       ((concatenate 'string hostname ":" (write-to-string port-number)) ()))
    (let* ((message (cl-protobufs.testing:make-hello-request :name "Neo"))
           (response (grpc:grpc-call channel "/net.grpc.examples.Greeter/SayHello"
                                     (cl-protobufs:serialize-to-bytes message))))
      (log:info "Response: ~A" (flexi-streams:octets-to-string (car response)))))
  (grpc:shutdown-grpc))
