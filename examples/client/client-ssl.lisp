;; Copyright 2016-2021 Google LLC
;;
;; Use of this source code is governed by an MIT-style
;; license that can be found in the LICENSE file or at
;; https://opensource.org/licenses/MIT.

;;  A simple test for gRPC Client in Common Lisp using SSL for channel authentication.

(defpackage #:testing-client-ssl
  (:use #:common-lisp)
  (:local-nicknames (#:testing #:cl-protobufs.lisp.grpc.integration-testing)
                    (#:testing-rpc #:cl-protobufs.lisp.grpc.integration-testing-rpc)
                    (#:log #:google.log)
                    (#:flag #:ace.flag))
  (:export #:main))

(in-package #:testing-client-ssl)

(flag:define hostname "localhost"
  "Name of server that will be connected to."
  :names ("hostname")
  :type STRING)

(flag:define port-number 8080
  "Port of server to establish connection to."
  :names ("port")
  :type INTEGER)

(flag:define root-cert-path ""
  "Path to root certificate file."
  :names ("root_cert_path")
  :type STRING)

(flag:define private-key-path ""
  "Path to private key file."
  :names ("private_key_path")
  :type STRING)

(flag:define cert-chain-path ""
  "Path to certificate chain file."
  :names ("cert_chain_path")
  :type STRING)

(defun main ()
  (google:init)
  (grpc:init-grpc)
  (let ((pem-root-certs (uiop:read-file-string (truename root-cert-path)))
        (private-key (uiop:read-file-string (truename private-key-path)))
        (cert-chain (uiop:read-file-string (truename cert-chain-path))))
    (grpc:with-ssl-channel
        (channel
         ((concatenate 'string hostname ":" (write-to-string port-number))
          (:pem-root-certs pem-root-certs
           :private-key private-key
           :cert-chain cert-chain)))

      ;; Unary streaming
      (let* ((message (testing:make-hello-request :name "Neo"))
             (response (testing-rpc:call-say-hello channel message)))
        (log:info "Response: ~A" (testing:hello-reply.message response)))

      ;; Server Streaming
      (let* ((message (testing:make-hello-request :name "Neo"
                                                  :num-responses 3))
             (response (testing-rpc:call-say-hello-server-stream channel message)))
        (loop for message in response
              do
           (log:info "Response: ~A" (testing:hello-reply.message message)))))
    (grpc:shutdown-grpc)))
