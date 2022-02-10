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

;; Functions for gRPC Client

(defun create-channel (target &optional (creds (cffi:null-pointer)) (args (cffi:null-pointer)))
  "A wrapper to create a channel for the client to TARGET with
additional args ARGS for client information. If CREDS is passed then a secure
channel will be created using CREDS else an insecure channel will be used."
  (c-grpc-client-new-channel creds target args))

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

(defun prepare-ops (ops message
                    &key
                    send-metadata send-message client-close
                    client-recv-status recv-metadata
                    recv-message)
  "Prepares OPS to send MESSAGE to the server. The keys SEND-METADATA
SEND-MESSAGE CLIENT-CLOSE CLIENT-RECV-STATUS RECV-METADATA RECV-MESSAGE
are all different types of ops that the user may want. Returns a plist
containing keys being the op type and values being the index."
  (let ((cur-index -1)
        ops-plist)
    (flet ((next-marker (message-type)
             (setf (getf ops-plist message-type) (incf cur-index))))

      (when send-metadata
        (make-send-metadata-op ops (cffi:null-pointer)
                               :count 0 :flag 0 :index (next-marker :send-metadata)))
      (when send-message
        (make-send-message-op ops message :index (next-marker :send-message)))
      (when client-close
        (make-client-close-op ops :flag 0 :index (next-marker :client-close)))
      (when client-recv-status
        (make-client-recv-status-op ops :flag 0 :index (next-marker :client-recv-status)))
      (when recv-metadata
        (make-recv-metadata-op ops :index (next-marker :recv-metadata)))
      (when recv-message
        (make-recv-message-op ops :flag 0 :index (next-marker :recv-message))))
    ops-plist))

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

(defmacro with-insecure-channel
    ((bound-channel address) &body body)
  "Creates a gRPC insecure channel to ADDRESS. Binds the channel to BOUND-CHANNEL, runs BODY,
and returns its values. After the body has run, the channel is destroyed."
  `(let ((,bound-channel (create-channel ,address)))
     (unwind-protect (progn ,@body)
       (grpc-channel-destroy ,bound-channel))))

(defmacro with-ssl-channel
    ((bound-channel (address (&key
                              (pem-root-certs nil)
                              (private-key nil)
                              (cert-chain nil)
                              (verify-peer-callback (cffi:null-pointer))
                              (verify-peer-callback-userdata (cffi:null-pointer))
                              (verify-peer-destruct (cffi:null-pointer)))))
     &body body)
  "Creates a gRPC secure channel to ADDRESS using SSL, which requires parameters to create the
SSL credentials and binds the channel to BOUND-CHANNEL. Then, BODY is run and returns its values.
After BODY has run, memory is freed for the SSL credentials and SSL credential options.

List containing the parameters values will correspond to fields of the
grpc_ssl_pem_key_cert_pair and grpc_ssl_verify_peer_options structs:
  (PEM-ROOT-CERTS<string> PRIVATE-KEY<string> CERT-CHAIN<string>
   VERIFY-PEER-CALLBACK<> PEER-CALLBACK-USERDATA<> VERIFY-PEER-DESTRUCT<>)

Allows the gRPC secure channel to be used in a memory-safe and concise manner."
  `(let* ((pem-root-certs (if ,pem-root-certs
                              (cffi:foreign-string-alloc ,pem-root-certs)
                              (cffi:null-pointer)))
          (private-key (if ,private-key
                           (cffi:foreign-string-alloc ,private-key)
                           (cffi:null-pointer)))
          (cert-chain (if ,cert-chain
                          (cffi:foreign-string-alloc ,cert-chain)
                          (cffi:null-pointer)))
          (ssl-pem-key-cert-pair (create-grpc-ssl-pem-key-cert-pair private-key cert-chain))
          (ssl-verify-peer-options
           (create-grpc-ssl-verify-peer-options ,verify-peer-callback
                                                ,verify-peer-callback-userdata
                                                ,verify-peer-destruct))
          (ssl-credentials
           (c-grpc-client-new-ssl-credentials
            pem-root-certs
            ssl-pem-key-cert-pair
            ssl-verify-peer-options))
          (,bound-channel (create-channel ,address ssl-credentials)))
     (unwind-protect (progn ,@body)
       (cffi:foreign-string-free pem-root-certs)
       (cffi:foreign-string-free private-key)
       (cffi:foreign-string-free cert-chain)
       (grpc-ssl-pem-key-cert-pair-delete ssl-pem-key-cert-pair)
       (grpc-ssl-verify-peer-options-delete ssl-verify-peer-options)
       (grpc-credentials-release ssl-credentials)
       (grpc-channel-destroy ,bound-channel))))

(defstruct call
  (c-call nil :type cffi:foreign-pointer)
  (c-tag nil :type cffi:foreign-pointer)
  (c-ops nil :type cffi:foreign-pointer)
  ;; This is a plist where the key is a keyword for a type of op
  ;; and the value is the index of that op in an op-array.
  (ops-plist nil :type list))

(defun receive-message (call)
  "Receive a message from the server for a CALL."
  (declare (type call call))
  (let* ((tag (cffi:foreign-alloc :int))
         (c-call (call-c-call call))
         (receive-op (create-new-grpc-ops 1))
         (ops-plist (prepare-ops receive-op nil :recv-message t))
         (call-code (call-start-batch c-call receive-op 1 tag)))
    (unless (eql call-code :grpc-call-ok)
      (grpc-ops-free receive-op 1)
      (cffi:foreign-free tag)
      (error 'grpc-call-error :call-error call-code))
    (when (completion-queue-pluck *completion-queue* tag)
      (cffi:foreign-free tag)
      (let* ((response-byte-buffer
              (get-grpc-op-recv-message receive-op (getf ops-plist :recv-message)))
             (message
              (unless (cffi:null-pointer-p response-byte-buffer)
                (loop for index from 0
                      to (1- (get-grpc-byte-buffer-slice-buffer-count
                              response-byte-buffer))
                      collecting (convert-grpc-slice-to-bytes
                                  (get-grpc-slice-from-grpc-byte-buffer
                                   response-byte-buffer index))
                      into message
                      finally
                   (grpc-byte-buffer-destroy response-byte-buffer)
                   (return message)))))
        (grpc-ops-free receive-op 1)
        (check-server-status
         (call-c-ops call)
         (getf (call-ops-plist call) :client-recv-status))
        message))))

(defun send-message (call bytes-to-send)
  "Send a message encoded in BYTES-TO-SEND to the server through a CALL."
  (declare (type call call))
  (let* ((c-call (call-c-call call))
         (tag (cffi:foreign-alloc :int))
         (send-op (create-new-grpc-ops 1))
         (grpc-slice
          (convert-grpc-slice-to-grpc-byte-buffer
           (convert-bytes-to-grpc-slice bytes-to-send)))
         (ops-plist (prepare-ops send-op grpc-slice :send-message t))
         (call-code (call-start-batch c-call send-op 1 tag)))
    (declare (ignore ops-plist))
    (unless (eql call-code :grpc-call-ok)
      (cffi:foreign-free tag)
      (grpc-ops-free send-op 1)
      (error 'grpc-call-error :call-error call-code))
    (let ((cqp-p (completion-queue-pluck *completion-queue* tag)))
      (grpc-ops-free send-op 1)
      (cffi:foreign-free tag)
      (check-server-status
       (call-c-ops call)
       (getf (call-ops-plist call) :client-recv-status))
      cqp-p)))

(defun client-close (call)
  "Close the client side of a CALL."
  (declare (type call call))
  (let* ((c-call (call-c-call call))
         (tag (cffi:foreign-alloc :int))
         (close-op (create-new-grpc-ops 1))
         (ops-plist (prepare-ops close-op nil :client-close t))
         (call-code (call-start-batch c-call close-op 1 tag)))
    (declare (ignore ops-plist))
    (unless (eql call-code :grpc-call-ok)
      (grpc-ops-free close-op 1)
      (error 'grpc-call-error :call-error call-code))
    (let ((ok (completion-queue-pluck *completion-queue* tag)))
      (cffi:foreign-free tag)
      (unless ok
        (check-server-status
         (call-c-ops call)
         (getf (call-ops-plist call) :client-recv-status)))
      (values))))

(defun check-server-status (ops receive-status-on-client-index)
  "Verify the server status is :grpc-status-ok. Requires the OPS containing the
RECEIVE_STATUS_ON_CLIENT op and RECEIVE-STATUS-ON-CLIENT-INDEX in the ops."
  (let ((server-status
         (recv-status-on-client-code ops receive-status-on-client-index)))
    (unless (eql server-status :grpc-status-ok)
      (error 'grpc-call-error :call-error server-status))))

(defconstant +num-ops-for-starting-call+ 3)

(defun start-grpc-call (channel service-method-name)
  "Start a grpc call. Requires a pointer to a grpc CHANNEL object, and a SERVICE-METHOD-NAME
string to direct the call to."
  (let* ((num-ops-for-sending-message +num-ops-for-starting-call+)
         (c-call (service-method-call channel service-method-name
                                      *completion-queue*))
         (ops (create-new-grpc-ops num-ops-for-sending-message))
         (tag (cffi:foreign-alloc :int))
         (ops-plist
          (prepare-ops ops nil :send-metadata t
                               :client-recv-status t
                               :recv-metadata t)))
    (call-start-batch c-call ops +num-ops-for-starting-call+ tag)
    (make-call :c-call c-call
               :c-tag tag
               :c-ops ops
               :ops-plist ops-plist)))

(defun free-call-data (call)
  "Free the call data stored in CALL-OBJ."
  (declare (type call call))
  (let* ((c-call (call-c-call call))
         (tag (call-c-tag call))
         (ops (call-c-ops call)))
    (completion-queue-pluck *completion-queue* tag)
    (cffi:foreign-free tag)
    (grpc-call-unref c-call)
    ;; The number of ops used to start the call,
    ;; see START-CALL.
    (grpc-ops-free ops +num-ops-for-starting-call+)))

(defun grpc-call (channel service-method-name bytes-to-send
                  server-stream client-stream)
  "Uses CHANNEL to call SERVICE-METHOD-NAME on the server with BYTES-TO-SEND
as the arguement to the method and returns the response<list of byte arrays>
from the server. If we are doing a client or bidirectional streaming call then
BYTES-TO-SEND should be a list of byte-vectors each containing a message to
send in a single call to the server. In the case of a server or bidirectional
call we return a list a list of byte vectors each being a response from the server,
otherwise it's a single byte vector list containing a single response."
  (let* ((call (start-grpc-call channel service-method-name)))
    (unwind-protect
         (progn
           (if client-stream
               (loop for bytes in bytes-to-send
                     do
                  (send-message call bytes))
               (send-message call bytes-to-send))
           (client-close call)
           (if server-stream
               (loop for message = (receive-message call)
                     while message
                     collect message)
               (receive-message call)))
      (free-call-data call))))
