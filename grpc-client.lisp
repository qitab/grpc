;;; Copyright 2021 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

;;;; Public Interface for gRPC

(in-package #:grpc)

(defvar *completion-queue* nil "The global completion queue used to
manage grpc calls.")

(define-condition grpc-call-error (error)
  ((call-error :initarg :call-error
             :initform nil
             :accessor call-error))
  (:report (lambda (condition stream)
     (format stream "GRPC CALL ERROR: ~A.~&" (call-error condition)))))

(defconstant +num-ops-for-sending-message+ 7
  "This is the minimum number of ops required for sending a message.")

(defconstant +position-of-recv-message-op+ 5
  "This is the position of the recv message op which contains the response from
the server.")

;; Functions for gRPC Client

(defun create-channel (target &optional creds (args (cffi:null-pointer)))
  "A wrapper to create a channel for the client to TARGET with
additional args ARGS for client information. If CREDS is passed then a secure
channel will be created using CREDS else an insecure channel will be used."
  (if creds
      (c-grpc-client-new-secure-channel creds target args)
      (c-grpc-client-new-insecure-channel target args)))

(defun service-method-call (channel call-name cq)
  "A wrapper to create a grpc_call pointer that will be used to call CALL-NAME
on the CHANNEL provided and store the result in the completion queue CQ."
  (cffi:foreign-funcall "lisp_grpc_channel_create_call"
                        :pointer channel :string call-name :pointer cq
                        :pointer))

(cffi:defcfun ("lisp_grpc_call_start_batch" call-start-batch)
  grpc-call-error
  (call :pointer)
  (ops :pointer)
  (num-ops :int)
  (tag :pointer))

(cffi:defcfun ("lisp_grpc_completion_queue_pluck" completion-queue-pluck)
  :bool
  (completion-queue :pointer)
  (tag :pointer))

;; Wrappers to create operations

(defun make-send-metadata-op (op metadata
                        &key count flag
                             index)
  "Sets OP[INDEX] to a Send Initial Metadata operation by adding metadata
METADATA, the count of metadata COUNT, and the flag FLAG."
  (cffi:foreign-funcall "lisp_grpc_make_send_metadata_op"
                        :pointer op
                        :pointer metadata
                        :int count
                        :int (convert-metadata-flag-to-integer flag)
                        :int index
                        :void))

(defun make-send-message-op (op message &key index)
  "Sets OP[INDEX] to a 'Send Message' operation that sends MESSAGE
to the server."
  (cffi:foreign-funcall "lisp_grpc_make_send_message_op"
                        :pointer op
                        :pointer message
                        :int index
                        :void))

(defun make-client-recv-status-op (op &key flag index)
  "Sets OP[INDEX] to a 'RECEIVE STATUS' operation, sets the FLAG of the op."
  (cffi:foreign-funcall "lisp_grpc_client_make_recv_status_op"
                        :pointer op
                        :int flag
                        :int index
                        :void))

(defun make-recv-message-op (op &key flag index)
  "Sets OP[INDEX] to a Receive Message operation with FLAG."
  (cffi:foreign-funcall "lisp_grpc_make_recv_message_op"
                        :pointer op
                        :int flag
                        :int index
                        :void))

(defun make-recv-metadata-op (op &key index)
  "Set OP[INDEX] to a Receive Initial Metadata operation with FLAG."
  (cffi:foreign-funcall "lisp_grpc_make_recv_metadata_op"
                        :pointer op
                        :int index
                        :void))

(defun make-client-close-op (op &key flag index)
  "Sets OP[INDEX] to a Send Close From Client operation with FLAG."
  (cffi:foreign-funcall "lisp_grpc_client_make_close_op"
                        :pointer op
                        :int flag
                        :int index
                        :void))

(defun prepare-ops-for-send-message (ops call tag message)
  "Prepares OPS to send MESSAGE to the server about CALL with TAG."
    (grpc::make-send-metadata-op ops (cffi:null-pointer)
                                 :count 0 :flag 0 :index 0)
    (grpc::make-send-message-op ops message :index 1)
    (grpc::make-client-close-op ops :flag 0 :index 2)
    (grpc::make-client-recv-status-op ops :flag 0 :index 3)
    (grpc::make-recv-metadata-op ops :index 4)
    (grpc::make-recv-message-op ops :flag 0 :index 5)
    (grpc::call-start-batch call ops 6 tag))

(cffi:defcfun ("lisp_grpc_op_recv_message" get-grpc-op-recv-message) :pointer
  (op :pointer)
  (index :int))

;; Auxiliary Functions

(cffi:defcfun ("create_new_grpc_ops" create-new-grpc-ops) :pointer
  "Creates a grpc_op* that is used to add NUM-OPS operations to,
these operation guide the interaction between the client and server."
  (num-ops :int))

(defun convert-metadata-flag-to-integer (flag)
  "Converts FLAG, a metadata symbol, to its integer equivalent."
  (case flag (grpc-write-through-flag #x4)
        (grpc-metadata-idempotent-flag #x10)
        (grpc-metadata-wait-for-ready-flag #x20)
        (grpc-metadata-cacheable-request-flag #x40)
        (grpc-metadata-wait-for-ready-explicitly-set-flag #x80)
        (grpc-metadata-corked-flag #x100)
        (otherwise flag)))

(defun convert-grpc-slice-to-grpc-byte-buffer (slice)
  "Takes a grpc_slice* SLICE and returns a pointer to the corresponding
grpc_byte_buffer*."
  (cffi:foreign-funcall "convert_grpc_slice_to_grpc_byte_buffer"
                        :pointer slice
                        :pointer))

(defun convert-bytes-to-grpc-slice (bytes)
  "Takes a list of bytes BYTES and returns a pointer to the corresponding
grpc_slice*."
  (let ((array (cffi:foreign-alloc :char :initial-contents bytes)))
    (cffi:foreign-funcall "convert_bytes_to_grpc_slice"
                          :pointer array
                          :size (length bytes)
                          :pointer)))

(defun convert-grpc-slice-to-bytes (slice)
  "Takes SLICE and returns its content as a vector of bytes."
  (let* ((slice-string-pointer (cffi:foreign-funcall
                                "convert_grpc_slice_to_string" :pointer slice
                                                               :pointer)))
  (cffi:foreign-array-to-lisp slice-string-pointer
                              (list :array :uint8
                                    (1+ (cffi:foreign-funcall
                                     "strlen"
                                     :pointer slice-string-pointer :int))))))

;; Exported Functions

(defun init-grpc ()
  "Initializes the grpc library and the global *completion-queue* so that
grpc functions can be used and the queue can be managed. Call before any gRPC
functions or macros are called and only call once."
  (cffi:foreign-funcall "grpc_init" :void)
  (unless *completion-queue*
    (setf *completion-queue* (grpc::c-grpc-completion-queue-create-for-pluck))))

(defun shutdown-grpc ()
  "Shuts down the grpc library which frees up any internal memory and
destroys *completion-queue*. Call when finished with all gRPC functions and
macros and only call once."
  (when *completion-queue*
    (cffi:foreign-funcall "grpc_completion_queue_shutdown"
                          :pointer *completion-queue*)
    (cffi:foreign-funcall "grpc_completion_queue_destroy"
                          :pointer *completion-queue*)
    (setf *completion-queue* nil))
  (cffi:foreign-funcall "grpc_shutdown"))

(defmacro with-loas2-channel
    ((bound-channel (address (&key
                                (desired-role (cffi:null-pointer))
                                (min-security-level :GRPC-SECURITY-NONE)
                                (serialized-server-authorization-policy
                                 (cffi:null-pointer))
                                (serialized-server-authorization-policy-length
                                 0)
                                (instance-info-required 0)))) &body body)
  "Creates a gRPC secure channel to ADDRESS using the
LOAS2 protocol, which requires parameters to create the LOAS2
credentials and binds the channel to BOUND-CHANNEL then runs BODY and returns
its values. After the body has run memory is freed for the LOAS2 credentials and
LOAS2 credential options, the channel is managed by the envelope (see go/envelope).

List containing the parameters values will
correspond to fields of grpc_loas2_credentials_options struct:
  (DESIRED-ROLE<string> MIN-SECURITY-LEVEL<grpc-security-level>
  SERIALIZED-SERVER-AUTHORIZATION-POLICY<string>
  SERIALIZED-SERVER-AUTHORIZATION-POLICY-LENGTH<int> INSTANCE-INFO-REQUIRED<int>)

Allows the gRPC secure channel to be used in a memory-safe and concise manner."
  `(let* ((loas2-credentials-options
            (create-grpc-loas2-credentials-options
             ,desired-role ,min-security-level
             ,serialized-server-authorization-policy
             ,serialized-server-authorization-policy-length
             ,instance-info-required))
          (loas2-credentials (c-grpc-client-new-loas2-credentials
                              loas2-credentials-options))
          (,bound-channel (create-channel ,address loas2-credentials)))
     (unwind-protect (progn ,@body)
       (grpc-loas2-credentials-options-delete loas2-credentials-options)
       (grpc-credentials-release loas2-credentials)
       (grpc-channel-destroy ,bound-channel))))

(defun grpc-call (channel service-method-name bytes-to-send)
  "Uses CHANNEL to call SERVICE-METHOD-NAME on the server with BYTES-TO-SEND
as the arguement to the method and returns the response<list of byte arrays> from the server."
  (let* ((call (grpc::service-method-call channel service-method-name
                                          *completion-queue*))
         (op (grpc::create-new-grpc-ops +num-ops-for-sending-message+))
         (tag (cffi:foreign-alloc :int))
         (call-code
           (prepare-ops-for-send-message
            op call tag
            (convert-grpc-slice-to-grpc-byte-buffer
             (convert-bytes-to-grpc-slice bytes-to-send))))
         (finally (if (equal call-code :GRPC-CALL-OK)
                      (completion-queue-pluck *completion-queue* tag)
                      (error 'grpc-call-error :call-error call-code)))
         (response-byte-buffer (when finally
                                 (grpc::get-grpc-op-recv-message
                                  op +position-of-recv-message-op+)))
         (response
           (when response-byte-buffer
             (loop for index from 0 to
                                    (1- (get-grpc-byte-buffer-slice-buffer-count
                                         response-byte-buffer))
                   collect (convert-grpc-slice-to-bytes
                            (get-grpc-slice-from-grpc-byte-buffer
                             response-byte-buffer index))))))
    (cffi:foreign-free tag)
    (grpc-byte-buffer-destroy response-byte-buffer)
    (grpc-call-unref call)
    (grpc-ops-free op +num-ops-for-sending-message+)
    response))
