;;; Copyright 2021 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

;;;; Public Interface for gRPC

(defpackage #:grpc.server
  (:use #:common-lisp)
  (:local-nicknames (#:log #:google.log))
  (:export
   ;; Server Functions
   #:main))
(in-package :grpc.server)

(defun main ()
  (google:init)
  (grpc:init-grpc)
  (let ((server (start-server grpc::*completion-queue*)))
    (loop for tag = (cffi:foreign-alloc :int)
          for metadata = (grpc::create-new-grpc-metadata-array)
          for call-details = (create-grpc-call-details)
          for grpc-call = (cffi:make-pointer 0)
          for call-code = (grpc-server-call server grpc-call call-details
                                            metadata grpc::*completion-queue* grpc::*completion-queue* tag)
          do
       (log:info "~A" call-code)
       (let*((grab-completion-queue (grpc::completion-queue-pluck grpc::*completion-queue* tag)))
         (log:info "~A" grab-completion-queue)))))

(declaim (notinline create-grpc-call-details))
(cffi:defcfun ("create_new_grpc_call_details" create-grpc-call-details)
              :pointer)

(declaim (notinline start-server))
(cffi:defcfun ("start_server" start-server)
              :pointer
              (cq :pointer))

(declaim (notinline grpc-server-call))
(cffi:defcfun ("grpc_server_call" grpc-server-call)
              :bool
              (server :pointer)
              (call :pointer)
              (details :pointer)
              (request-metadata :pointer)
              (cq-bound :pointer)
              (cq-notify :pointer)
              (tag :pointer))