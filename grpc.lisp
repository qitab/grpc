;; Copyright 2016-2021 Google LLC
;;
;; Use of this source code is governed by an MIT-style
;; license that can be found in the LICENSE file or at
;; https://opensource.org/licenses/MIT.

;;  A wrapper for using gRPC in Common Lisp.

(defpackage #:grpc
  (:use #:common-lisp)
  (:export
   ;; Client Functions
   #:init-grpc
   #:shutdown-grpc
   #:with-insecure-channel
   #:grpc-call))
