;;;; -*- Mode: LISP; Syntax: Ansi-Common-Lisp; Base: 10; -*-

#.(unless (or #+asdf3.1 (version<= "3.1" (asdf-version)))
    (error "You need ASDF >= 3.1 to load this system correctly."))

(defsystem :grpc
  :author "Jonathan Godbout <jgodbout@google.com>"
  :version (:read-file-form "version.sexp")
  :description "Lisp wrapper for gRPC"
  :license "MIT"
  :depends-on (:cl-protobufs :cffi)
  :serial t
  :components ((:file "grpc")
               (:file "libraries")
               (:file "wrappers")
               (:file "client")
               (:file "protobuf-integration")))
