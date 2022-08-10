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
  :in-order-to ((test-op (test-op :grpc/tests)))
  :components ((:file "grpc")
               (:file "libraries")
               (:file "wrappers")
               (:file "client")
               (:file "server")
               (:file "protobuf-integration")))

(defsystem :grpc/tests
  :name "Protobufs Tests"
  :author "Jonathan Godbout"
  :version "0.1"
  :licence "MIT-style"
  :maintainer '("Jon Godbout" "Carl Gay" "Nick Groszewski")
  :description      "Test code for gRPC for Common Lisp"
  :long-description "Test code for gRPC for Common Lisp"
  :depends-on (:grpc :clunit2 :flexi-streams :bordeaux-threads)
  :serial t
  :pathname "tests/"
  :components
  ((:module "packages"
    :serial t
    :pathname ""
    :components ((:file "pkgdcl")))
   (:module "root-suite"
    :serial t
    :pathname ""
    :components ((:file "root-suite")))
   (:module "integration-test"
    :serial t
    :pathname ""
    :depends-on ("packages")
    :components ((:file "integration-test"))))
  :perform (test-op (o c)
                    (uiop:symbol-call '#:grpc.test '#:run-all)))
