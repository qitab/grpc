;;; Copyright 2026 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

;;; ASDF systems for the example gRPC clients, used by run_integration_tests.sh
;;; in the open-source CI. (google3 builds these from the BUILD file instead.)

(defsystem :grpc-examples
  :description "Generated Lisp code for helloworld.proto, shared by the example clients."
  :license "MIT"
  :defsystem-depends-on (:cl-protobufs.asdf)
  :depends-on (:grpc)
  :components ((:protobuf-source-file "helloworld")))

(defsystem :grpc-examples/insecure
  :description "Example gRPC client using an insecure channel."
  :license "MIT"
  :depends-on (:grpc-examples :ace.flag)
  :components ((:file "client-insecure")))

(defsystem :grpc-examples/ssl
  :description "Example gRPC client using an SSL channel."
  :license "MIT"
  :depends-on (:grpc-examples :ace.flag)
  :components ((:file "client-ssl")))
