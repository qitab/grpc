;;; Copyright 2021 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

;;;; Lisp wrappers

(in-package #:grpc)

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

;; gRPC Client Channel wrappers
(defun c-grpc-client-new-channel (creds target args)
  "Creates a secure channel to TARGET using the passed-in
credentials CREDS. Additional channel level configuration MAY be provided
by grpc_channel_ARGS."
  (cffi:foreign-funcall "grpc_channel_create"
                        :string target
                        :pointer creds
                        :pointer args
                        :pointer))

(defun c-grpc-client-new-default-channel (call-creds user-provided-audience)
  "Creates default credentials to connect to a google gRPC service.
WARNING: Do NOT use this credentials to connect to a non-google service as
this could result in an oauth2 token leak. The security level of the
resulting connection is GRPC_PRIVACY_AND_INTEGRITY.

 - CALL-CREDS is an optional parameter will be attached to the
   returned channel credentials object.

 - USER-PROVIDED-AUDIENCE is an optional field for user to override the
   audience in the JWT token if used."
  (cffi:foreign-funcall "grpc_google_default_credentials_create"
                        :pointer call-creds
                        :string user-provided-audience
                        :pointer))

;; gRPC Client Credentials wrappers

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

(defun get-call-method (buffer)
  "Get a lisp-vector of bytes from the grpc_slice at INDEX
i of grpc_byte_buffer BUFFER."
  (let ((c-bytes
         (cffi:foreign-funcall "grpc_call_method"
                               :pointer buffer
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
  (let ((array (cffi:foreign-alloc :char :initial-contents bytes)))
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
