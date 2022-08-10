;;; Copyright 2021 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

;;;; Load foreign libraries.

(in-package #:grpc)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (cffi:define-foreign-library grpc-client-wrapper
    ;; Load the C wrapper directly from the source directory.
    (t (:default #.(namestring
                    (asdf:system-relative-pathname "grpc" "grpc")))))
  (cffi:load-foreign-library 'grpc-client-wrapper))
