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
                    ;; begin-internal
                    (#:log #:google.log)
                    ;; end-internal
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
  ;; begin-internal
  (google:init)
  ;; end-internal
  (grpc:init-grpc)
  (grpc:with-insecure-channel
      (channel (concatenate 'string hostname ":" (write-to-string port-number)))
    (let* ((message (cl-protobufs.testing:make-hello-request :name "Neo"))
           (response (grpc:grpc-call channel "/testing.Greeter/SayHello"
                                     (cl-protobufs:serialize-to-bytes message))))
      (format nil "Response: ~A" (flexi-streams:octets-to-string (car response)))))
  (grpc:shutdown-grpc))
