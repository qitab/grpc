;;; Copyright 2021 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

;;;; Lisp wrappers

(in-package #:grpc)

(defmacro wrap-c-fun (wrapper-name c-fun-name
                      wrapper-args-with-type c-fun-args-with-type
                      &optional docstring)
  "Creates a wrapper with symbol WRAPPER-NAME that uses WRAPPER-ARGS-WITH-TYPE
 over a C function called C-FUN-NAME that uses WRAPPER-ARGS-WITH-TYPE and
 C-FUN-ARGS-WITH-TYPE and with a docstring DOCSTRING. Use the macro as follows:

 (wrap-c-fun wrapped-fun-symbol c-fun<a string> docstring
                                        ((:int arg1))
                                        ((:pointer cffi:null-pointer))
                                        docstring)

 Now use the wrapped function as follows (wrapped-fun-symbol 6)

 The types can be any defined in cffi and with libffi can be defined with
 defcstruct and defcenum too."
  (progn
    (if docstring
        (setf (documentation wrapper-name 'function) docstring))
    (flet ((unnest (li)
           (let (ret)
             (dolist (p li)
               (setf ret (append ret p)))
             ret)))
    `(defun ,wrapper-name ,(mapcar #'second wrapper-args-with-type)
       (cffi:foreign-funcall ,c-fun-name
                        ,@(unnest wrapper-args-with-type)
                        ,@(unnest c-fun-args-with-type)
                        :pointer)))))

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

;; gRPC Client Channel wrappers
(wrap-c-fun c-grpc-client-new-insecure-channel
            "grpc_insecure_channel_create"
            ((:string target)
            (:pointer args))
            ((:pointer (cffi:null-pointer)))
            "Creates a client channel to TARGET.")

(wrap-c-fun c-grpc-client-new-secure-channel
            "grpc_secure_channel_create"
            ((:pointer creds)
             (:string target)
             (:pointer args))
            ((:pointer (cffi:null-pointer)))
            "Creates a secure channel to TARGET using the passed-in
 credentials CREDS. Additional channel level configuration MAY be provided
by grpc_channel_args.")

(wrap-c-fun c-grpc-client-new-default-channel
            "grpc_google_default_credentials_create"
            ((:pointer call-creds)
             (:string user-provided-audience))
             ()
            "Creates default credentials to connect to a google gRPC service.
   WARNING: Do NOT use this credentials to connect to a non-google service as
   this could result in an oauth2 token leak. The security level of the
   resulting connection is GRPC_PRIVACY_AND_INTEGRITY.

   - CALL-CREDS is an optional parameter will be attached to the
   returned channel credentials object.

   - USER-PROVIDED-AUDIENCE is an optional field for user to override the
   audience in the JWT token if used.")

;; gRPC Client Credentials wrappers

(wrap-c-fun c-grpc-client-new-ssl-credentials
            "grpc_ssl_credentials_create_ex"
            ((:pointer pem-roots-certs)
             (:pointer pem-key-cert-pair)
             (:pointer verify-options))
            ((:pointer (cffi:null-pointer)))
            "Creates an SSL credentials object.
   The security level of the resulting connection is GRPC_PRIVACY_AND_INTEGRITY.
   - PEM-ROOTS-CERTS is the PEM encoding of the server root certificates.
   - PEM-KEY-CERT-PAIR is a pointer on the object containing client's private
     key and certificate chain.
   - VERIFY-OPTIONS holds additional options controlling how peer certificates
     are verified.")

(wrap-c-fun c-grpc-client-new-metadata-credentials
            "grpc_metadata_credentials_create_from_plugin"
            ((:pointer plugin)
             (grpc-security-level min-security-level))
            ()
            "This method creates a local channel credential object. The security
 level of the resulting connection is GRPC_PRIVACY_AND_INTEGRITY for UDS and
  GRPC_SECURITY_NONE for LOCAL_TCP. It is used for experimental purpose
  for now and subject to change.")

(cffi:defcfun ("create_grpc_loas2_credentials_options"
               create-grpc-loas2-credentials-options) :pointer
  (desired-role :string)
  (min-security-level grpc-security-level)
  (serialized-server-authorization-policy :string)
  (serialized-server-authorization-policy-length :size)
  (instance-info-required :int))

(wrap-c-fun c-grpc-client-new-loas2-credentials
            "grpc_loas2_credentials_create"
            ((:pointer options))
            ()
            "This method creates a LOAS2 channel credential object.")

(cffi:defcfun ("delete_grpc_loas2_credentials_options" grpc-loas2-credentials-options-delete)
    :void
  "Deletes OPTIONS, a grpc_loas2_credentials_options object."
   (options :pointer))

(wrap-c-fun c-grpc-client-new-alts-credentials
            "grpc_alts_credentials_create"
            ((:pointer options)
             (grpc-security-level min-security-level))
            ()
            "This method creates an ALTS channel credential object. The security
  level of the resulting connection is GRPC_PRIVACY_AND_INTEGRITY.")

(wrap-c-fun c-grpc-client-new-access-token-credentials
            "grpc_access_token_credentials_create"
            ((:string access_token))
            ((:pointer (cffi:null-pointer)))
            "Creates an Oauth2 Access Token credentials with an access token
that was acquired by an out of band mechanism.")

(wrap-c-fun c-grpc-client-new-composite-call-credentials
            "grpc_composite_call_credentials_create"
            ((:pointer creds1)
             (:pointer creds2))
            ((:pointer (cffi:null-pointer)))
            "Creates a composite call credentials object.")

(wrap-c-fun c-grpc-client-new-composite-channel-credentials
            "grpc_composite_channel_credentials_create"
            ((:pointer channel-creds)
             (:pointer call-creds))
            ((:pointer (cffi:null-pointer)))
            "Creates a composite channel credentials object. The security level of
  resulting connection is determined by CHANNEL-CREDS.")

(wrap-c-fun c-grpc-client-new-local-credentials
            "grpc_local_credentials_create"
            ((:pointer type))
            ()
            "This method creates a local channel credential object. The security
 level of the resulting connection is GRPC_PRIVACY_AND_INTEGRITY for UDS and
   GRPC_SECURITY_NONE for LOCAL_TCP.")

(wrap-c-fun c-grpc-client-new-tls-credentials
            "grpc_tls_credentials_create"
            ((:pointer options))
            ()
            "Creates a TLS channel credential object based on the
  grpc_tls_credentials_options specified by callers. The
  security level of the resulting connection is GRPC_PRIVACY_AND_INTEGRITY.")

(wrap-c-fun c-grpc-client-new-google-default-credentials
            "grpc_google_default_credentials_create"
            ((:pointer options))
            ()
            "Creates default credentials to connect to a google gRPC service.
   WARNING: Do NOT use this credentials to connect to a non-google service as
   this could result in an oauth2 token leak. The security level of the
   resulting connection is GRPC_PRIVACY_AND_INTEGRITY.")

(wrap-c-fun c-grpc-client-new-google-compute-engine-credentials
            "grpc_google_compute_engine_credentials_create"
            ()
            ((:pointer (cffi:null-pointer)))
            "Creates a compute engine credentials object for connecting to Google.
   WARNING: Do NOT use this credentials to connect to a non-google service as
   this could result in an oauth2 token leak.")

(wrap-c-fun c-grpc-client-new-xds-credentials
            "grpc_xds_credentials_create"
            ((:pointer fallback-credentials))
            ()
            "This method creates an xDS channel credentials object.")

(wrap-c-fun c-grpc-client-new-external-account-credentials
            "grpc_external_account_credentials_create"
            ((:string json-string)
             (:string scopes-string))
            ()
            "Builds External Account credentials.
 - JSON-STRING is the JSON string containing the credentials options.
 - SCOPES-STRING contains the scopes to be binded with the credentials.")

(wrap-c-fun c-grpc-client-new-refresh-token-credentials
            "grpc_google_refresh_token_credentials_create"
            ((:string json-refresh))
            ((:pointer (cffi:null-pointer)))
            "Creates an Oauth2 Refresh Token credentials object for connecting to Google.
   WARNING: Do NOT use this credentials to connect to a non-google service as
   this could result in an oauth2 token leak.
   - JSON-REFRESH-TOKEN is the JSON string containing the refresh token itself
     along with a client_id and client_secret")

(wrap-c-fun c-grpc-client-new-google-iam-credentials
            "grpc_google_iam_credentials_create"
            ((:string authorization-token)
             (:string authorirt-selector))
            ((:pointer (cffi:null-pointer)))
            "Creates an IAM credentials object for connecting to Google.")

(wrap-c-fun c-grpc-client-new-sts-credentials
            "grpc_sts_credentials_create"
            ((:pointer options))
            ((:pointer (cffi:null-pointer)))
            "Creates an STS credentials following the STS Token Exchanged specifed in the
   IETF draft https://tools.ietf.org/html/draft-ietf-oauth-token-exchange-16.")

;; gRPC Server Credentials

(wrap-c-fun c-grpc-server-new-loas2-credentials
            "grpc_loas2_server_credentials_create"
            ((:pointer options))
            ()
            "This method creates a LOAS2 channel credential object.")

(wrap-c-fun c-grpc-server-new-ssl-credentials
            "grpc_ssl_server_credentials_create_with_options"
            ((:pointer options))
            ()
            "Creates an SSL server_credentials object using the provided options struct.")

(wrap-c-fun c-grpc-server-new-ssl-credentials-options
            "grpc_ssl_server_credentials_create_options_using_config"
            ((grpc-ssl-client-certificate-request-type client-certificate-request)
             (:pointer certificate-config))
            ()
            " Creates an options object using a certificate config. Use this method when
   the certificates and keys of the SSL server will not change during the
   server's lifetime.")

(wrap-c-fun c-grpc-server-new-local-credentials
            "grpc_local_server_credentials_create"
            ((:pointer type))
            ()
            "This method creates a local server credential object")

(wrap-c-fun c-grpc-server-new-tls-credentials
            "grpc_tls_server_credentials_create"
            ((:pointer options))
            ()
            "Creates a TLS server credential object based on the
  grpc_tls_credentials_options specified by callers.")

(wrap-c-fun c-grpc-server-new-xds-credentials
            "grpc_xds_server_credentials_create"
            ((:pointer fallback-credentials))
            ()
            "his method creates an xDS server credentials object.")

(wrap-c-fun c-grpc-server-new-alts-credentials
            "grpc_alts_server_credentials_create"
            ((:pointer fallback-credentials))
            ()
            "This method creates an ALTS server credential object.")

(wrap-c-fun c-grpc-completion-queue-create-for-pluck
            "grpc_completion_queue_create_for_pluck"
            ()
            ((:pointer (cffi:null-pointer)))
            "This wrapper creates a completion_queue* that is used to start a batch
of operation and check the success.")

;; Wrapped grpc-client.cc functions

(cffi:defcfun ("grpc_ops_free" grpc-ops-free) :void
  "Deletes and destroys all memory in fields of OPS upto index SIZE
before freeing ops."
   (ops :pointer) (size :int))

(cffi:defcfun ("grpc_channel_credentials_release" grpc-credentials-release)
  :void
  "Releases CREDENTIALS."
  (credentials :pointer))

(cffi:defcfun ("grpc_byte_buffer_destroy" grpc-byte-buffer-destroy) :void
  "Destroys BYTE-BUFFER, a grpc_byte_buffer object."
  (byte-buffer :pointer))

(cffi:defcfun ("grpc_call_unref" grpc-call-unref) :void
  "Unrefs CALL, a grpc_call object."
  (call :pointer))

(cffi:defcfun ("grpc_channel_destroy" grpc-channel-destroy) :void
  "Closes and destroys CHANNEL, a grpc_channel object."
  (channel :pointer))

(cffi:defcfun ("create_new_grpc_metadata_array" create-new-grpc-metadata-array)
    :pointer)

(cffi:defcfun ("create_empty_grpc_byte_buffer" create-grpc-byte-buffer)
    :pointer)

(cffi:defcfun ("create_empty_grpc_slice" create-grpc-slice) :pointer)

(cffi:defcfun ("create_empty_grpc_status_code" create-grpc-status-code)
    :pointer)

(cffi:defcfun ("convert_string_to_grpc_slice" convert-string-to-grpc-slice)
  :pointer
  (str :string))

(cffi:defcfun ("get_grpc_slice_from_grpc_byte_buffer"
               get-grpc-slice-from-grpc-byte-buffer)
  :pointer
  (buf :pointer)
  (index :int))

(cffi:defcfun ("grpc_byte_buffer_slice_buffer_count"
               get-grpc-byte-buffer-slice-buffer-count) :int
  (op :pointer))
