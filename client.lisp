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

(defun service-method-call (channel call-name cq &optional (timeout -1.0d0))
  "A wrapper to create a grpc_call pointer that will be used to call CALL-NAME
on the CHANNEL provided and store the result in the completion queue CQ.
TIMEOUT is the timeout for the call in seconds. A negative value means no timeout."
  (cffi:foreign-funcall "lisp_grpc_channel_create_call"
                        :pointer channel :string call-name :pointer cq
                        :double (float timeout 0.0d0)
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
  (let ((creds (gensym "CREDS")))
    `(let* ((,creds (grpc::grpc-insecure-credentials-create))
            (,bound-channel (create-channel ,address ,creds)))
       (unwind-protect (progn ,@body)
         (grpc-credentials-release ,creds)
         (grpc-channel-destroy ,bound-channel)))))

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

(defun client-close (call)
  "Close the client side of a CALL."
  (declare (type call call))
  (let ((c-call (call-c-call call))
        (cq (call-completion-queue call)))
    (cffi:with-foreign-objects ((tag :int)
                                (close-op '(:struct grpc-op)))
      (let ((ops-plist (prepare-ops close-op :client-close t)))
        (declare (ignore ops-plist))
        (let ((ok (unwind-protect
                       (let ((call-code (call-start-batch c-call close-op 1 tag)))
                         (unless (eql call-code :grpc-call-ok)
                           (error 'grpc-call-error :call-error call-code))
                         (completion-queue-pluck cq tag))
                    (grpc-ops-clear close-op 1))))
          (unless ok (check-server-status call))
          (values))))))

(defun check-server-status (call)
  "Check the server status with data from a CALL object"
  (declare (type call call))
  (unless (call-status-plucked-p call)
    (completion-queue-pluck (call-completion-queue call) (call-c-tag call))
    (setf (call-status-plucked-p call) t))
  (%check-server-status
   call
   (call-c-ops call)
   (getf (call-ops-plist call) :client-recv-status)))

(defun %check-server-status (call ops receive-status-on-client-index)
  "Verify the server status is :grpc-status-ok. Requires the OPS containing the
RECEIVE_STATUS_ON_CLIENT op and RECEIVE-STATUS-ON-CLIENT-INDEX in the ops."
  (let ((server-status
          (recv-status-on-client-code ops receive-status-on-client-index)))
    (setf (call-status-checked-p call) t)
    (unless (eql server-status :grpc-status-ok)
      (error 'grpc-call-error :call-error server-status))))

(defconstant +num-ops-for-starting-call+ 3)

(defstruct (async-call (:include call))
  "Tracks an in-flight or completed non-blocking gRPC client call."
  (callback nil :type (or null function))
  (transform #'identity :type function)
  (lock (bordeaux-threads:make-lock "async-call-lock"))
  (condvar (bordeaux-threads:make-condition-variable :name "async-call-cv"))
  (completed-p nil :type boolean)
  (response nil)
  (error nil))

(defun finalize-async-call (async-call raw-response call-err)
  "Applies ASYNC-CALL's transform to RAW-RESPONSE and invokes its callback
when CALL-ERR is nil, records the result or CALL-ERR, and wakes any threads
waiting in ASYNC-CALL-WAIT."
  (declare (type async-call async-call))
  (let ((final-response nil))
    (unless call-err
      (handler-case
          (setf final-response
                (funcall (async-call-transform async-call) raw-response))
        (error (e)
          (setf call-err e))))
    (when (and (async-call-callback async-call) (not call-err))
      (handler-case
          (funcall (async-call-callback async-call) final-response)
        (error (e)
          (setf call-err e))))
    (bordeaux-threads:with-lock-held ((async-call-lock async-call))
      (setf (async-call-response async-call) final-response
            (async-call-error async-call) call-err
            (async-call-completed-p async-call) t)
      (bordeaux-threads:condition-notify (async-call-condvar async-call)))))

(defun complete-async-call (async-call success-p)
  "Extracts the response and status from ASYNC-CALL after completion on a
GRPC_CQ_NEXT queue with event status SUCCESS-P, frees C resources via
FREE-CALL-DATA, and finalizes ASYNC-CALL."
  (declare (type async-call async-call))
  (setf (call-status-plucked-p async-call) t)
  (let ((raw-response nil)
        (call-err nil))
    (handler-case
        (unwind-protect
             (if success-p
                 (setf raw-response
                       (extract-recv-message (call-c-ops async-call)
                                             (call-ops-plist async-call)))
                 (progn
                   (setf (call-status-checked-p async-call) t)
                   (error 'grpc-call-error :call-error :grpc-call-error)))
          (free-call-data async-call))
      (error (e)
        (setf call-err e)))
    (finalize-async-call async-call raw-response call-err)))

(defun poll-async-completion-queue (cq)
  "Background polling loop that waits for completed events on CQ via
grpc_completion_queue_next and dispatches their completion handlers."
  (loop
    (multiple-value-bind (event-type tag success-p)
        (completion-queue-next cq -1.0d0)
      (case event-type
        (:grpc-queue-shutdown
         (return))
        (:grpc-op-complete
         (let ((async-call
                 (bordeaux-threads:with-lock-held (*async-calls-lock*)
                   (let ((key (cffi:pointer-address tag)))
                     (prog1 (gethash key *pending-async-calls*)
                       (remhash key *pending-async-calls*))))))
           (when async-call
             (complete-async-call async-call success-p))))))))

(defun ensure-async-completion-queue ()
  "Returns *ASYNC-COMPLETION-QUEUE*, creating it and starting the background
polling thread if necessary."
  (bordeaux-threads:with-lock-held (*async-calls-lock*)
    (unless *async-completion-queue*
      (let ((cq (c-grpc-completion-queue-create-for-next)))
        (unless (and cq (not (cffi:null-pointer-p cq)))
          (error "Failed to create gRPC async completion queue"))
        (setf *async-completion-queue* cq
              *async-poller-thread*
              (bordeaux-threads:make-thread
               (lambda ()
                 (poll-async-completion-queue cq))
               :name "gRPC Async Completion Queue Poller"))))
    *async-completion-queue*))

(defun async-call-ready-p (async-call)
  "Returns T if ASYNC-CALL has completed, or NIL if it is still in flight."
  (declare (type async-call async-call))
  (bordeaux-threads:with-lock-held ((async-call-lock async-call))
    (async-call-completed-p async-call)))

(defun async-call-wait (async-call)
  "Blocks until ASYNC-CALL completes and returns its response, or signals any
error that occurred during the call."
  (declare (type async-call async-call))
  (bordeaux-threads:with-lock-held ((async-call-lock async-call))
    (loop until (async-call-completed-p async-call)
          do (bordeaux-threads:condition-wait
              (async-call-condvar async-call)
              (async-call-lock async-call)))
    (when (async-call-error async-call)
      (error (async-call-error async-call)))
    (async-call-response async-call)))

(defun %init-client-call (call channel service-method-name client-context cq
                          &key send-message client-close recv-message)
  "Initializes CALL's C call, tag, and ops array on CQ for CHANNEL and
SERVICE-METHOD-NAME with CLIENT-CONTEXT and optional SEND-MESSAGE, CLIENT-CLOSE,
and RECV-MESSAGE ops."
  (let* ((num-ops (+ +num-ops-for-starting-call+
                     (if send-message 1 0)
                     (if client-close 1 0)
                     (if recv-message 1 0)))
         (c-call (service-method-call channel service-method-name cq
                                      (if client-context
                                          (context-deadline client-context)
                                          -1.0d0)))
         (ops (create-new-grpc-ops num-ops))
         (tag (cffi:foreign-alloc :int))
         (ops-plist
           (prepare-ops ops
                        :send-metadata (or (and client-context
                                                (context-metadata client-context))
                                           t)
                        :send-message (and send-message
                                           (convert-bytes-to-grpc-byte-buffer
                                            send-message))
                        :client-close client-close
                        :client-recv-status t
                        :recv-metadata t
                        :recv-message recv-message)))
    (setf (call-c-call call) c-call
          (call-c-tag call) tag
          (call-c-ops call) ops
          (call-c-cq call) cq
          (call-method-name call) service-method-name
          (call-ops-plist call) ops-plist
          (call-context call) client-context
          (call-initial-metadata-sent-p call) t)
    (values call num-ops)))

(defun start-grpc-call (channel service-method-name client-context)
  "Start a grpc call. Requires a pointer to a grpc CHANNEL object, and a SERVICE-METHOD-NAME
string to direct the call to. CLIENT-CONTEXT provides optional metadata and deadline."
  (let ((cq (c-grpc-completion-queue-create-for-pluck)))
    (multiple-value-bind (call num-ops)
        (%init-client-call (make-call :owns-cq-p t)
                           channel service-method-name client-context cq)
      (let ((call-code (call-start-batch (call-c-call call) (call-c-ops call)
                                         num-ops (call-c-tag call))))
        (unless (eql call-code :grpc-call-ok)
          (setf (call-status-plucked-p call) t
                (call-status-checked-p call) t)
          (free-call-data call)
          (error 'grpc-call-error :call-error call-code)))
      call)))

(defun grpc-async-call (channel service-method-name bytes-to-send
                        client-context server-stream client-stream
                        &key callback (transform #'identity))
  "Starts a non-blocking gRPC call over CHANNEL for SERVICE-METHOD-NAME with
BYTES-TO-SEND, CLIENT-CONTEXT, SERVER-STREAM, and CLIENT-STREAM flags, and
returns an ASYNC-CALL object immediately. For unary RPCs, the call batch is
submitted directly to the GRPC_CQ_NEXT completion queue and polled in the
background via grpc_completion_queue_next. When the call completes, TRANSFORM
is applied to the raw response and CALLBACK (if provided) is invoked with the
result."
  (let ((async-call (make-async-call :method-name service-method-name
                                     :context client-context
                                     :server-stream-p (not (null server-stream))
                                     :client-stream-p (not (null client-stream))
                                     :callback callback
                                     :transform transform)))
    (if (and (not server-stream) (not client-stream))
        (multiple-value-bind (call num-ops)
            (%init-client-call async-call channel service-method-name
                               client-context (ensure-async-completion-queue)
                               :send-message bytes-to-send
                               :client-close t
                               :recv-message t)
          (let ((tag-key (cffi:pointer-address (call-c-tag call))))
            (bordeaux-threads:with-lock-held (*async-calls-lock*)
              (setf (gethash tag-key *pending-async-calls*) call))
            (let ((call-code (call-start-batch (call-c-call call) (call-c-ops call)
                                               num-ops (call-c-tag call))))
              (unless (eql call-code :grpc-call-ok)
                (bordeaux-threads:with-lock-held (*async-calls-lock*)
                  (remhash tag-key *pending-async-calls*))
                (setf (call-status-plucked-p call) t
                      (call-status-checked-p call) t)
                (free-call-data call)
                (error 'grpc-call-error :call-error call-code)))
            call))
        (progn
          (bordeaux-threads:make-thread
           (lambda ()
             (let ((raw-response nil)
                   (call-err nil))
               (handler-case
                   (setf raw-response
                         (grpc-call channel service-method-name bytes-to-send
                                    client-context server-stream client-stream))
                 (error (e)
                   (setf call-err e)))
               (finalize-async-call async-call raw-response call-err)))
           :name "gRPC Async Streaming Call")
          async-call))))

(defun grpc-call (channel service-method-name bytes-to-send
                  client-context server-stream client-stream &key callback)
  "Uses CHANNEL to call SERVICE-METHOD-NAME on the server with BYTES-TO-SEND
as the arguement to the method and returns the response<list of byte arrays>
from the server. If we are doing a client or bidirectional streaming call then
BYTES-TO-SEND should be a list of byte-vectors each containing a message to
send in a single call to the server. In the case of a server or bidirectional
call we return a list a list of byte vectors each being a response from the server,
otherwise it's a single byte vector list containing a single response.
If CALLBACK is provided, starts a non-blocking call and returns an ASYNC-CALL."
  (if callback
      (grpc-async-call channel service-method-name bytes-to-send
                       client-context server-stream client-stream
                       :callback callback)
      (let* ((call (start-grpc-call channel service-method-name client-context)))
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
          (free-call-data call)))))
