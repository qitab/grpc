;;; Copyright 2021 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

;;;; Lisp wrappers

(in-package #:grpc)

;; Conditions
(define-condition grpc-call-error (error)
  ((call-error :initarg :call-error
               :initform nil
               :accessor call-error))
  (:report (lambda (condition stream)
             (format stream "GRPC CALL ERROR: ~A.~&" (call-error condition)))))

;; Globals

(defvar *completion-queue* nil "The global completion queue used to
manage grpc calls.")

;; gRPC Enums
(cffi:defcenum grpc-security-level
  "Security levels of grpc transport security. It represents an inherent
property of a backend connection and is determined by a channel credential
used to create the connection."
  :GRPC-SECURITY-MIN
  :GRPC-SECURITY-NONE
  :GRPC-INTEGRITY-ONLY
  :GRPC-PRIVACY-AND-INTEGRITY
  :GRPC-SECURITY-MAX)

(cffi:defcenum grpc-ssl-client-certificate-request-type
  "SSL Client Certificate Request Types are how the gRPC Server should handle
SSL Authentication"
  :GRPC-SSL-DONT-REQUEST-CLIENT-CERTIFICATE
  :GRPC-SSL-REQUEST-CLIENT-CERTIFICATE-BUT-DONT-VERIFY
  :GRPC-SSL-REQUEST-CLIENT-CERTIFICATE-AND-VERIFY
  :GRPC-SSL-REQUEST-AND-REQUIRE-CLIENT-CERTIFICATE-BUT-DONT-VERIFY
  :GRPC-SSL-REQUEST-AND-REQUIRE-CLIENT-CERTIFICATE-AND-VERIFY)

(cffi:defcenum grpc-call-error
  "This enum represents all the possible return values from
grpc_call_start_batch."
  :GRPC-CALL-OK
  :GRPC-CALL-ERROR
  :GRPC-CALL-ERROR-NOT-ON-SERVER
  :GRPC-CALL-ERROR-NOT-ON-CLIENT
  :GRPC-CALL-ERROR-ALREADY-ACCEPTED
  :GRPC-CALL-ERROR-ALREADY-INVOKED
  :GRPC-CALL-ERROR-NOT-INVOKED
  :GRPC-CALL-ERROR-ALREADY-FINISHED
  :GRPC-CALL-ERROR-TOO-MANY-OPERATIONS
  :GRPC-CALL-ERROR-INVALID-FLAGS
  :GRPC-CALL-ERROR-INVALID-METADATA
  :GRPC-CALL-ERROR-INVALID-MESSAGE
  :GRPC-CALL-ERROR-NOT-SERVER-COMPLETION-QUEUE
  :GRPC-CALL-ERROR-BATCH-TOO-BIG
  :GRPC-CALL-ERROR-PAYLOAD-TYPE-MISMATCH
  :GRPC-CALL-ERROR-COMPLETION-QUEUE-SHUTDOWN)

(cffi:defcenum grpc-status-code
  "The grpc-status-code enum values"
  :GRPC-STATUS-OK
  :GRPC-STATUS-CANCELLED
  :GRPC-STATUS-UNKNOWN
  :GRPC-STATUS-INVALID-ARGUMENT
  :GRPC-STATUS-DEADLINE-EXCEEDED
  :GRPC-STATUS-NOT-FOUND
  :GRPC-STATUS-ALREADY-EXISTS
  :GRPC-STATUS-PERMISSION-DENIED
  :GRPC-STATUS-RESOURCE-EXHAUSTED
  :GRPC-STATUS-FAILED-PRECONDITION
  :GRPC-STATUS-ABORTED
  :GRPC-STATUS-OUT-OF-RANGE
  :GRPC-STATUS-UNIMPLEMENTED
  :GRPC-STATUS-INTERNAL
  :GRPC-STATUS-UNAVAILABLE
  :GRPC-STATUS-DATA-LOSS
  :GRPC-STATUS-UNAUTHENTICATED)

;; gRPC Credentials wrappers

(cffi:defcfun ("create_grpc_ssl_pem_key_cert_pair"
               create-grpc-ssl-pem-key-cert-pair) :pointer
  (private-key :string)
  (cert-chain :string))

(cffi:defcfun ("delete_grpc_ssl_pem_key_cert_pair" grpc-ssl-pem-key-cert-pair-delete)
  :void
  "Deletes KEYPAIR, a grpc_ssl_pem_key_cert_pair object."
  (keypair :pointer))

(cffi:defcfun ("create_grpc_ssl_verify_peer_options"
               create-grpc-ssl-verify-peer-options) :pointer
  (verify-peer-callback :pointer)
  (verify-peer-callback-userdata :pointer)
  (verify-peer-destruct :pointer))

(cffi:defcfun ("delete_grpc_ssl_verify_peer_options" grpc-ssl-verify-peer-options-delete)
  :void
  "Deletes OPTIONS, a grpc_ssl_verify_peer_options object."
  (options :pointer))

(defun c-grpc-client-new-ssl-credentials
    (pem-roots-certs pem-key-cert-pair verify-options)
  "Creates an SSL credentials object.
The security level of the resulting connection is GRPC_PRIVACY_AND_INTEGRITY.
 - PEM-ROOTS-CERTS is the PEM encoding of the server root certificates.
 - PEM-KEY-CERT-PAIR is a pointer on the object containing client's private
   key and certificate chain.
 - VERIFY-OPTIONS holds additional options controlling how peer certificates
   are verified."
  (cffi:foreign-funcall "grpc_ssl_credentials_create_ex"
                        :string pem-roots-certs
                        :pointer pem-key-cert-pair
                        :pointer verify-options
                        :pointer (cffi-sys:null-pointer)
                        :pointer))

(defun c-grpc-client-new-metadata-credentials (plugin min-security-level)
  "This method creates a local channel credential object. The security
level of the resulting connection is GRPC_PRIVACY_AND_INTEGRITY for UDS and
GRPC_SECURITY_NONE for LOCAL_TCP. It is used for experimental purpose
for now and subject to change."
  (cffi:foreign-funcall "grpc_metadata_credentials_create_from_plugin"
                        :pointer plugin grpc-security-level min-security-level :pointer))

(defun c-grpc-client-new-alts-credentials (options min-security-level)
  "This method creates an ALTS channel credential object. The security
level of the resulting connection is GRPC_PRIVACY_AND_INTEGRITY."
  (cffi:foreign-funcall "grpc_alts_credentials_create"
                        :pointer options grpc-security-level min-security-level :pointer))

(defun c-grpc-client-new-access-token-credentials (access_token)
  "Creates an Oauth2 Access Token credentials with an access token
that was acquired by an out of band mechanism."
  (cffi:foreign-funcall "grpc_access_token_credentials_create"
                        :string access_token
                        :pointer (cffi-sys:null-pointer)
                        :pointer))

(defun c-grpc-client-new-composite-call-credentials (creds1 creds2)
  "Creates a composite call credentials object."
  (cffi:foreign-funcall "grpc_composite_call_credentials_create"
                        :pointer creds1
                        :pointer creds2
                        :pointer (cffi-sys:null-pointer)
                        :pointer))

(defun c-grpc-client-new-composite-channel-credentials
    (channel-creds call-creds)
  "Creates a composite channel credentials object. The security level of
resulting connection is determined by CHANNEL-CREDS."
  (cffi:foreign-funcall "grpc_composite_channel_credentials_create"
                        :pointer channel-creds
                        :pointer call-creds
                        :pointer (cffi-sys:null-pointer)
                        :pointer))

(defun c-grpc-client-new-local-credentials (type)
  "This method creates a local channel credential object. The security
level of the resulting connection is GRPC_PRIVACY_AND_INTEGRITY for UDS and
GRPC_SECURITY_NONE for LOCAL_TCP."
  (cffi:foreign-funcall "grpc_local_credentials_create" :pointer type :pointer))

(defun c-grpc-client-new-tls-credentials (options)
  "Creates a TLS channel credential object based on the grpc_tls_credentials_options
specified by callers. The security level of the resulting connection is
GRPC_PRIVACY_AND_INTEGRITY."
  (cffi:foreign-funcall "grpc_tls_credentials_create" :pointer options :pointer))

(defun c-grpc-client-new-google-default-credentials (options)
  "Creates default credentials to connect to a google gRPC service.
WARNING: Do NOT use this credentials to connect to a non-google service as
this could result in an oauth2 token leak. The security level of the
resulting connection is GRPC_PRIVACY_AND_INTEGRITY."
  (cffi:foreign-funcall "grpc_google_default_credentials_create"
                        :pointer options :pointer))

(defun c-grpc-client-new-google-compute-engine-credentials ()
  "Creates a compute engine credentials object for connecting to Google.
WARNING: Do NOT use this credentials to connect to a non-google service as
this could result in an oauth2 token leak."
  (cffi:foreign-funcall "grpc_google_compute_engine_credentials_create"
                        :pointer (cffi-sys:null-pointer) :pointer))

(defun c-grpc-client-new-xds-credentials (fallback-credentials)
  "This method creates an xDS channel credentials object."
  (cffi:foreign-funcall "grpc_xds_credentials_create"
                        :pointer fallback-credentials :pointer))

(defun c-grpc-client-new-external-account-credentials (json-string scopes-string)
  "Builds External Account credentials.
 - JSON-STRING is the JSON string containing the credentials options.
 - SCOPES-STRING contains the scopes to be binded with the credentials."
  (cffi:foreign-funcall "grpc_external_account_credentials_create"
                        :string json-string :string scopes-string :pointer))

(defun c-grpc-client-new-refresh-token-credentials (json-refresh)
  "Creates an Oauth2 Refresh Token credentials object for connecting to Google.

WARNING: Do NOT use this credentials to connect to a non-google service as
         this could result in an oauth2 token leak.

   - JSON-REFRESH-TOKEN is the JSON string containing the refresh token itself
     along with a client_id and client_secret"
  (cffi:foreign-funcall "grpc_google_refresh_token_credentials_create"
                        :string json-refresh :pointer (cffi-sys:null-pointer) :pointer))

(defun c-grpc-client-new-google-iam-credentials (authorization-token authorirt-selector)
  "Creates an IAM credentials object for connecting to Google."
  (cffi:foreign-funcall "grpc_google_iam_credentials_create"
                        :string authorization-token
                        :string authorirt-selector
                        :pointer (cffi-sys:null-pointer)
                        :pointer))

(defun c-grpc-client-new-sts-credentials (options)
  "Creates an STS credentials following the STS Token Exchanged specifed in the
   IETF draft https://tools.ietf.org/html/draft-ietf-oauth-token-exchange-16."
  (cffi:foreign-funcall "grpc_sts_credentials_create"
                        :pointer options :pointer (cffi-sys:null-pointer) :pointer))

;; gRPC Server Credentials

(defun c-grpc-server-new-ssl-credentials (options)
  "Creates an SSL server_credentials object using the provided options struct."
  (cffi:foreign-funcall "grpc_ssl_server_credentials_create_with_options"
                        :pointer options :pointer))

(defun c-grpc-server-new-ssl-credentials-options (client-certificate-request certificate-config)
  "Creates an options object using a certificate config. Use this method when
the certificates and keys of the SSL server will not change during the
server's lifetime."
  (cffi:foreign-funcall
   "grpc_ssl_server_credentials_create_options_using_config"
   grpc-ssl-client-certificate-request-type client-certificate-request
   :pointer certificate-config :pointer))

(defun c-grpc-server-new-local-credentials (type)
  "This method creates a local server credential object"
  (cffi:foreign-funcall "grpc_local_server_credentials_create"
                        :pointer type :pointer))

(defun c-grpc-server-new-tls-credentials (options)
  "Creates a TLS server credential object based on the grpc_tls_credentials_options
 specified by callers."
  (cffi:foreign-funcall "grpc_tls_server_credentials_create"
                        :pointer options :pointer))

(defun c-grpc-server-new-xds-credentials (fallback-credentials)
  "his method creates an xDS server credentials object."
  (cffi:foreign-funcall "grpc_xds_server_credentials_create"
                        :pointer fallback-credentials :pointer))

(defun c-grpc-server-new-alts-credentials (fallback-credentials)
  "This method creates an ALTS server credential object."
  (cffi:foreign-funcall "grpc_alts_server_credentials_create"
                        :pointer fallback-credentials :pointer))

(defun c-grpc-completion-queue-create-for-pluck ()
  "This wrapper creates a completion_queue* that is used to start a batch
of operation and check the success."
  (cffi:foreign-funcall "grpc_completion_queue_create_for_pluck"
                        :pointer (cffi-sys:null-pointer) :pointer))

;; Wrapped grpc-client.cc functions

(cffi:defcfun ("grpc_ops_free" grpc-ops-free) :void
  "Deletes and destroys all memory in fields of OPS upto index SIZE
before freeing ops."
  (ops :pointer) (size :int))

(cffi:defcfun ("grpc_channel_credentials_release" grpc-credentials-release)
  :void
  "Releases CREDENTIALS."
  (credentials :pointer))

(cffi:defcfun ("grpc_byte_buffer_destroy" grpc-byte-buffer-destroy ) :void
  "Destroys BYTE-BUFFER, a grpc_byte_buffer object."
  (byte-buffer :pointer))

(cffi:defcfun ("grpc_call_unref" grpc-call-unref) :void
  "Unrefs CALL, a grpc_call object."
  (call :pointer))

(cffi:defcfun ("grpc_channel_destroy" grpc-channel-destroy) :void
  "Closes and destroys CHANNEL, a grpc_channel object."
  (channel :pointer))

(cffi:defcfun ("create_new_grpc_metadata_array" create-new-grpc-metadata-array )
  :pointer)

(cffi:defcfun ("create_empty_grpc_byte_buffer" create-grpc-byte-buffer )
  :pointer)

(cffi:defcfun ("create_empty_grpc_slice" create-grpc-slice) :pointer)

(cffi:defcfun ("create_empty_grpc_status_code" create-grpc-status-code)
  :pointer)

(cffi:defcfun ("lisp_grpc_op_get_status" recv-status-on-client-code) grpc-status-code
  (ops :pointer)
  (index :int))

(cffi:defcfun ("convert_string_to_grpc_slice" convert-string-to-grpc-slice)
  :pointer
  (str :string))

(cffi:defcfun ("get_grpc_slice_from_grpc_byte_buffer"
               get-grpc-slice-from-grpc-byte-buffer )
  :pointer
  (buf :pointer)
  (index :int))

(cffi:defcfun ("grpc_byte_buffer_slice_buffer_count"
               get-grpc-byte-buffer-slice-buffer-count ) :int
  (op :pointer))

(cffi:defcfun ("grpc_insecure_credentials_create"
               grpc-insecure-credentials-create)
  :pointer)

(cffi:defcfun ("grpc_insecure_server_credentials_create"
               grpc-insecure-server-credentials-create)
  :pointer)

(cffi:defcfun ("delete_grpc_metadata_array" metadata-destroy)
  :void
  (metadata :pointer))

(cffi:defcfun ("free_grpc_slice" free-slice)
  :void
  (slice :pointer))

(defun get-call-method (call-details)
  "Get the call method from a grpc_call_details BUFFER."
  (let ((c-bytes
         (cffi:foreign-funcall "grpc_call_method"
                               :pointer call-details
                               :pointer)))
    (prog1 (cffi:foreign-string-to-lisp c-bytes)
      (cffi:foreign-funcall "free"
                            :pointer c-bytes
                            :void))))

(defun get-bytes-from-grpc-byte-buffer (buffer index)
  "Get a lisp-vector of bytes from the grpc_slice at INDEX
i of grpc_byte_buffer BUFFER."
  (let ((c-bytes
         (cffi:foreign-funcall "convert_grpc_byte_buffer_to_bytes"
                               :pointer buffer
                               :int index
                               :pointer)))
    (prog1 (cffi:foreign-array-to-lisp c-bytes
                                       (list :array :uint8
                                             (cffi:foreign-funcall
                                              "strlen"
                                              :pointer c-bytes :int)))
      (cffi:foreign-funcall "free"
                            :pointer c-bytes
                            :void))))


(defun convert-bytes-to-grpc-byte-buffer (bytes)
  "Given a lisp-vector of BYTES convert them to a grpc_byte_buffer."
  (let ((array (cffi:foreign-alloc :unsigned-char :initial-contents bytes)))
    (prog1
        (cffi:foreign-funcall "convert_bytes_to_grpc_byte_buffer"
                               :pointer array
                               :int (length bytes)
                               :pointer)
      (cffi:foreign-free array))))

(defun convert-metadata-flag-to-integer (flag)
  "Converts FLAG, a metadata symbol, to its integer equivalent."
  (case flag (grpc-write-through-flag #x4)
        (grpc-metadata-idempotent-flag #x10)
        (grpc-metadata-wait-for-ready-flag #x20)
        (grpc-metadata-cacheable-request-flag #x40)
        (grpc-metadata-wait-for-ready-explicitly-set-flag #x80)
        (grpc-metadata-corked-flag #x100)
        (otherwise flag)))

(defstruct method-details
  (name "" :type string)
  (serializer #'identity :type function)
  (deserializer #'identity :type function)
  (action #'identity :type function)
  (server-stream nil :type boolean)
  (client-stream nil :type boolean))

;; Completion Queue Functions

(cffi:defcfun ("lisp_grpc_call_start_batch" call-start-batch )
  grpc-call-error
  (call :pointer)
  (ops :pointer)
  (num-ops :int)
  (tag :pointer))

(cffi:defcfun ("lisp_grpc_completion_queue_pluck" completion-queue-pluck )
  :bool
  (completion-queue :pointer)
  (tag :pointer))

;; Wrappers to create operations

(cffi:defcfun ("lisp_grpc_op_recv_message" get-grpc-op-recv-message ) :pointer
  (op :pointer)
  (index :int))

(cffi:defcfun ("create_new_grpc_ops" create-new-grpc-ops) :pointer
  "Creates a grpc_op* that is used to add NUM-OPS operations to,
these operation guide the interaction between the client and server."
  (num-ops :int))

(defun make-metadata (metadata)
  "Sets OP[INDEX] to a Send Initial Metadata operation by adding metadata
METADATA, the count of metadata COUNT, and the flag FLAG."
  (let* ((arr-size (length metadata))
         (metadata
          (loop for (key value) in metadata
                collect
                (cffi:foreign-funcall "lisp_make_grpc_metadata"
                                      :string key
                                      :string value)))
         (l-arr (make-array (list arr-size)
                            :initial-contents metadata)))
    (cffi:with-foreign-array (arr l-arr (list :array :int64 arr-size))
      (cffi:foreign-funcall "create_new_grpc_metadata_array_with_data"
                            :pointer arr
                            :size arr-size))))

(defun make-send-metadata-op (op metadata
                              &key count flag
                              index)
  "Sets OP[INDEX] to a Send Initial Metadata operation by adding metadata
METADATA, the count of metadata COUNT, and the flag FLAG."
  (let ((metadata-ptr (if metadata
                          (make-metadata metadata)
                          (cffi:null-pointer))))

    (cffi:foreign-funcall "lisp_grpc_make_send_metadata_op"
                          :pointer op
                          :int index
                          :pointer metadata-ptr
                          :int count
                          :int (convert-metadata-flag-to-integer flag)
                          :void)))

(defun make-send-message-op (op message &key index)
  "Sets OP[INDEX] to a 'Send Message' operation that sends MESSAGE
to the server."
  (cffi:foreign-funcall "lisp_grpc_make_send_message_op"
                        :pointer op
                        :int index
                        :pointer message
                        :void))

(defun make-client-recv-status-op (op &key flag index)
  "Sets OP[INDEX] to a 'RECEIVE STATUS' operation, sets the FLAG of the op."
  (cffi:foreign-funcall "lisp_grpc_client_make_recv_status_op"
                        :pointer op
                        :int index
                        :int flag
                        :void))

(defun make-recv-message-op (op &key flag index)
  "Sets OP[INDEX] to a Receive Message operation with FLAG."
  (cffi:foreign-funcall "lisp_grpc_make_recv_message_op"
                        :pointer op
                        :int index
                        :int flag
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
                        :int index
                        :int flag
                        :void))

(defun make-send-status-from-server-op (op &key metadata count status flag index)
  "Sets OP[INDEX] to a Send Status from server operation by adding metadata
METADATA, the server STATUS, the count of metadata COUNT, and the flag FLAG."
  (cffi:foreign-funcall "lisp_grpc_make_send_status_from_server_op"
                        :pointer op
                        :int index
                        :pointer metadata
                        :int count
                        grpc-status-code status
                        :int flag
                        :void))

(defun make-recv-close-on-server-op (op &key cancelled flag index)
  "Sets OP[INDEX] to a Receive Close on Server operation by adding cancelled CANCELLED
and the flag FLAG"
  (cffi:foreign-funcall "lisp_grpc_server_make_close_op"
                        :pointer op
                        :int index
                        :pointer cancelled
                        :int flag
                        :void))


(defun prepare-ops (ops
                    &key
                    send-metadata send-message client-close
                    client-recv-status recv-metadata
                    recv-message server-recv-close server-send-status)
  "Prepares OPS to send MESSAGE to the server. The keys SEND-METADATA
SEND-MESSAGE CLIENT-CLOSE CLIENT-RECV-STATUS RECV-METADATA RECV-MESSAGE
SERVER-RECV-CLOSE SERVER-SEND-STATUS are all different types of ops that the user may
want. Returns a plist containing keys being the op type and values being the index."
  (let ((cur-index -1)
        ops-plist)
    (flet ((next-marker (message-type)
             (setf (getf ops-plist message-type) (incf cur-index))))

      (when send-metadata
        (make-send-metadata-op ops send-metadata
                               :count 0 :flag 0 :index (next-marker :send-metadata)))
      (when send-message
        (make-send-message-op ops send-message :index (next-marker :send-message)))
      (when client-close
        (make-client-close-op ops :flag 0 :index (next-marker :client-close)))
      (when client-recv-status
        (make-client-recv-status-op ops :flag 0 :index (next-marker :client-recv-status)))
      (when recv-metadata
        (make-recv-metadata-op ops :index (next-marker :recv-metadata)))
      (when recv-message
        (make-recv-message-op ops :flag 0 :index (next-marker :recv-message)))
      (when server-recv-close
        (make-recv-close-on-server-op ops :cancelled (cffi:foreign-alloc :int) :flag 0 :index (next-marker :server-close)))
      (when server-send-status
        (make-send-status-from-server-op ops :metadata (cffi:null-pointer) :count 0 :status server-send-status :flag 0 :index (next-marker :server-send-status))))
    ops-plist))

;; Conversion, deletion functions

;; Hack since :size defctype doesn't work in
;; cffi:foreign-funcall externally
(cffi:defctype :size #+64-bit :uint64 #+32-bit :uint32)

(defun convert-bytes-to-grpc-slice (bytes)
  "Takes a list of bytes BYTES and returns a pointer to the corresponding
grpc_slice*."
  (let ((array (cffi:foreign-alloc :unsigned-char :initial-contents bytes)))
    (cffi:foreign-funcall "convert_bytes_to_grpc_slice"
                          :pointer array
                          :size (length bytes)
                          :pointer)))

(defun convert-grpc-slice-to-bytes (slice)
  "Takes SLICE and returns its content as a vector of bytes."
  (let* ((slice-string-pointer
          (cffi:foreign-funcall
           "convert_grpc_slice_to_string" :pointer slice
                                          :pointer)))
    (cffi:foreign-array-to-lisp slice-string-pointer
                                (list :array :uint8
                                      (cffi:foreign-funcall
                                           "strlen"
                                           :pointer slice-string-pointer :int)))))

;; General gRPC functions

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

;; Shared structures

(defstruct call
  (c-call nil :type cffi:foreign-pointer)
  (c-tag nil :type cffi:foreign-pointer)
  (c-ops nil :type cffi:foreign-pointer)
  (method-name "" :type string)
  ;; This is a plist where the key is a keyword for a type of op
  ;; and the value is the index of that op in an op-array.
  (ops-plist nil :type list)
  (context nil :type (or null client-context server-context)))

(defstruct context
  (deadline -1 :type integer)
  (metadata nil :type list))

(defun receive-message (call)
  "Receive a message from the client for a CALL."
  (declare (type call call))
  (let* ((tag (cffi:foreign-alloc :int))
         (c-call (call-c-call call))
         (receive-op (create-new-grpc-ops 1))
         (ops-plist (prepare-ops receive-op :recv-message t))
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
                      collecting (get-bytes-from-grpc-byte-buffer
                                  response-byte-buffer index)
                        into message
                      finally
                   (grpc-byte-buffer-destroy response-byte-buffer)
                   (return message)))))
        (grpc-ops-free receive-op 1)
        message))))

(defun send-message (call bytes-to-send)
  "Send the GRPC_OP_SEND_MESSAGE message encoded in BYTES-TO-SEND to the server through a CALL"
  (declare (type call call))
  (let* ((num-ops 1)
         (c-call (call-c-call call))
         (tag (cffi:foreign-alloc :int))
         (ops (create-new-grpc-ops num-ops))
         (grpc-slice
          (convert-bytes-to-grpc-byte-buffer bytes-to-send))
         (context (call-context call))
         (ops-plist (prepare-ops
                     ops
                     :send-message grpc-slice
                     :send-metadata (and context
                                         (context-metadata context))))
         (call-code (call-start-batch c-call ops num-ops tag)))
    (declare (ignore ops-plist))
    (unless (eql call-code :grpc-call-ok)
      (cffi:foreign-free tag)
      (grpc-ops-free ops num-ops)
      (error 'grpc-call-error :call-error call-code))
    (let ((cqp-p (completion-queue-pluck *completion-queue* tag)))
      (grpc-ops-free ops num-ops)
      (cffi:foreign-free tag)
      cqp-p)))

(defun free-call-data (call)
  "Free the call data stored in CALL-OBJ."
  (declare (type call call))
  (let* ((c-call (call-c-call call))
         (tag (call-c-tag call))
         (ops (call-c-ops call)))
    (unless (cffi:null-pointer-p ops)
      (completion-queue-pluck *completion-queue* tag)
      (cffi:foreign-free tag)
      (grpc-ops-free ops (/ (length (call-ops-plist call)) 2)))
    (grpc-call-unref c-call)))
