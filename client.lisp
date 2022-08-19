;;; Copyright 2021 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

;;;; Public Interface for gRPC

(in-package #:grpc)

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

;; Functions for gRPC Client

(defun create-channel (target &optional
                              (creds (cffi:null-pointer))
                              (args (cffi:null-pointer)))
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


;; Auxiliary Functions

(defun convert-grpc-slice-to-grpc-byte-buffer (slice)
  "Takes a grpc_slice* SLICE and returns a pointer to the corresponding
grpc_byte_buffer*."
  (cffi:foreign-funcall "convert_grpc_slice_to_grpc_byte_buffer"
                        :pointer slice
                        :pointer))

;; Exported Functions

(defmacro with-insecure-channel
    ((bound-channel address) &body body)
  "Creates a gRPC insecure channel to ADDRESS. Binds the channel to BOUND-CHANNEL, runs BODY,
and returns its values. After the body has run, the channel is destroyed."
  `(let ((,bound-channel (create-channel
                          ,address (grpc::grpc-insecure-credentials-create))))
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

(defun receive-message (call)
  "Receive a message from the server for a CALL."
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
           (convert-bytes-to-grpc-byte-buffer bytes-to-send))
         (ops-plist (prepare-ops send-op :send-message grpc-slice))
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
         (ops-plist (prepare-ops close-op :client-close t))
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
           (prepare-ops ops :send-metadata t
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
    (unless (cffi:null-pointer-p ops)
      (completion-queue-pluck *completion-queue* tag)
      (cffi:foreign-free tag))
    (grpc-call-unref c-call)
    ;; The number of ops used to start the call,
    ;; see START-CALL.
    (grpc-ops-free ops (/ (length (call-ops-plist call)) 2))))

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
