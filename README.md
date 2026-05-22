# gRPC Library in Common Lisp

[![SBCL-Tests](https://github.com/qitab/grpc/actions/workflows/SBCL-test.yml/badge.svg)](https://github.com/qitab/grpc/actions/workflows/SBCL-test.yml)
[![CCL-Tests](https://github.com/qitab/grpc/actions/workflows/CCL-test.yml/badge.svg)](https://github.com/qitab/grpc/actions/workflows/CCL-test.yml)



## Overview

This package defines a [gRPC](https://grpc.io/) client library for Common Lisp.
It wraps gRPC core functions with CFFI calls and uses those core functions to
create and use a client. client.lisp contains all the necessary functions to
create a gRPC client by creating channels (connections between client and
server) and calls (requests to a server).

Currently there is support for synchronous and streaming calls over

SSL, and insecure channels.

Support for implementing gRPC servers is fully supported, including unary and streaming services.

## Usage

To create a client, a channel must first be created. Depending on the expected
authentication mechanism (or lack thereof), different channel creation macros
are available.

### Channel Creation

#### Insecure Channels

If using an insecure channel, use the `with-insecure-channel` macro. This macro
expects a symbol to bind the channel to and the server address.

```lisp
(with-insecure-channel (channel "localhost:8080")
;; Code that uses channel
...)
```

#### SSL Channels

If using an SSL channel, use the `with-ssl-channel` macro. This macro
expects a symbol to bind the channel, the server address and
certificate data to make the call.

```lisp
(with-ssl-channel (channel
                    ("localhost:8080"
                      (:pem-root-certs pem-root-certs
                       :private-key private-key
                       :cert-chain cert-chain)))
;; Code that uses channel
...)
```



### Sending RPC Requests

Once a channel has been created, RPC requests to the server can occur.
It is possible to send binary data directly over `gRPC` but most applications using
`gRPC` will expect a Protocol Buffer encoded message. For details on Protocol buffer
integration please see [Protocol Buffer Integration](#protocol-buffer-integration).

A message can be sent directly through `gRPC` using `grpc-call`.
This expects the channel that was previously created, the service name and method to be
called, and the request message serialized to bytes. The `grpc-call` method also
takes `server-stream` and `client-stream` arguments which state whether the message
should use server or client side streaming as discussed in
[Types of Services](#types-of-services)

```lisp
(grpc:grpc-call channel
                "/serviceName/ServiceMethod"
                serialized-message server-stream client-stream)
;; Returns the response a list of byte vectors for each response
```

### Types of Services

An RPC can support any of `unary`, `mono-directional`, or `bidirectional` streaming.
This must be decided beforehand by the server and client.

There are two different types of `mono-directional-streaming` RPC's:
1. Server Side Streaming.
2. Client Side Streaming.

See https://grpc.io/docs/what-is-grpc/core-concepts/#rpc-life-cycle  for details.

#### Unary RPC

A unary RPC sends one message and receives one message.
The `grpc-call` function takes in a single vector for `bytes-to-send`
and return a single octet-vector.

#### Server Side Streaming RPC

A server side streaming RPC sends one message and receives multiple messages.
The `grpc-call` function takes in a single vector for `bytes-to-send`
and return a list of octet-vectors corresponding to the received messages.

#### Client Side Streaming RPC

A client side streaming RPC sends some number of messages and receives a single message.
The `grpc-call` function takes in a list of vectors for `bytes-to-send`
and returns an octet-vector corresponding to the received message.

#### Bidirectional Streaming RPC

A bidirectional streaming RPC sends any number of messages and receives any number of messages.
The `grpc-call` function takes in a list of vectors for `bytes-to-send`
and returns a list of octet-vectors corresponding to the received messages.

### Protocol Buffer Integration

gRPC can work with or without Protocol Buffer support. With that said,
it is common to use a Protocol Buffer library in conjunction with gRPC.
We have implemented support for the `cl-protobufs` library.

The Qitab team provides supports `cl-protobufs` but doesn't guarantee continued support
for other data format libraries.

To use gRPC with `cl-protobufs` you must load `cl-protobufs` and `gRPC` with
`grpc-protobuf-integration.lisp` into your running lisp image.

Example:

Define a protocol buffer service with methods as:

```proto
package testing;

message HelloRequest {
  optional string name = 1;
}

message HelloReply {
  optional string message = 1;
}

service Greeter {
  // Receives a HelloRequest and responds with a HelloReply.
  rpc SayHello(HelloRequest) returns (HelloReply) {}
  // Receive a HelloRequest requesting some number of responses in num_responses
  // and response with a HelloReply num_responses times.
  rpc SayHelloServerStream(HelloRequest) returns (stream HelloReply) {}
  // Receive a number of requests and concatenate the name field of each
  // HelloRequest. Return the final string in HelloReply.
  rpc SayHelloClientStream(stream HelloRequest) returns (HelloReply) {}
  // Receive a number of HelloRequest requesting some number of responses in num_responses.
  // Respond to each HelloRequest with a HelloReply num_responses times.
  rpc SayHelloBidirectionalStream(stream HelloRequest) returns (stream HelloReply) {}
}
```

We create two packages:

* `cl-protobufs.testing`
* `cl-protobufs.testing-rpc`

The package `cl-protobufs.testing` contains the `hello-request` and `hello-reply`  protocol
buffer messages.

#### One Shot Client Calls.

The package `cl-protobufs.testing-rpc` contains a stub for `call-say-hello`.
A message can be sent to a server implementing the `Greeter` service with:

```lisp
  (grpc:with-insecure-channel
      (channel (concatenate 'string hostname ":" (write-to-string port-number)))
    (let* ((request (cl-protobufs.testing:make-hello-request :name "Neo"))
           (response (cl-protobufs.testing-rpc:call-say-hello channel message)))
      ...))
```

If the service implements client-side streaming `message` should be a list
of `hello-request` messages to be sent to the server. If the service implements
server-side streaming then response will contain a list of `hello-reply`
messages.

#### Asynchronous Client Streaming

For streaming calls we create:

-   `cl-protobufs.testing-rpc:<service-name>/start`
-   `cl-protobufs.testing-rpc:<service-name>/send`
-   `cl-protobufs.testing-rpc:<service-name>/receive`
-   `cl-protobufs.testing-rpc:<service-name>/close`
-   `cl-protobufs.testing-rpc:<service-name>/cleanup`

functions.

We will use `SayHelloBidirectionalStream` service as an example below.

```lisp
(testing-rpc:say-hello-bidirectional-stream/start channel)
```

Takes in a `channel` object and returns a `call` object that the user must keep
until the call is `closed` and `cleanup` is called.

##### Idiomatic Client Stream Management

Rather than manually ensuring `/close` and `/cleanup` in `unwind-protect` blocks, prefer using the `with-client-stream` macro:

```lisp
(grpc:with-client-stream (call (testing-rpc:say-hello-bidirectional-stream/start channel))
  (testing-rpc:say-hello-bidirectional-stream/send call message)
  (testing-rpc:say-hello-bidirectional-stream/receive call))
```

`with-client-stream` automatically ensures the stream is half-closed (if not already closed) and all C memory is safely freed upon exiting the block.

##### Unified Generic Stream API

In addition to the per-method helper functions generated by `cl-protobufs` (e.g., `say-hello-bidirectional-stream/send`), `grpc` provides a unified generic stream API that operates on the `call` object directly across all services and methods:

-   `(grpc:stream-send call message)`: Sends a Protobuf message over the stream.
-   `(grpc:stream-receive call)`: Receives a Protobuf message from the stream. Blocks until a message arrives, returning `NIL` on EOF.
-   `(grpc:stream-close call)`: Half-closes the stream.
-   `(grpc:stream-cleanup call)`: Cleans up a client call object.

These generic functions inspect the `call` object to correctly handle both client-side and server-side streaming semantics. E.g., `grpc:stream-send` can be used by both a client sending requests and a server sending replies.

#### Server Implementation (including Streaming)

To implement a gRPC server using `cl-protobufs`, you define methods on the generated generic functions in the `-rpc` package.

##### Unary Server Methods

For unary methods, define a method taking the request message and the call object:

```lisp
(defmethod cl-protobufs.testing-rpc:say-hello ((request cl-protobufs.testing:hello-request) call)
  (declare (ignore call))
  (cl-protobufs.testing:make-hello-reply :message (concatenate 'string "Hello " (cl-protobufs.testing:hello-request.name request))))
```

##### Streaming Server Methods

For streaming calls, `cl-protobufs` generates additional server streaming helper functions:

-   `cl-protobufs.testing-rpc:<service-name>/server-send`
-   `cl-protobufs.testing-rpc:<service-name>/server-receive`
-   `cl-protobufs.testing-rpc:<service-name>/server-receive-close`
-   `cl-protobufs.testing-rpc:<service-name>/server-send-status`

Alternatively, you can use the unified generic stream API (`grpc:stream-send`, `grpc:stream-receive`).

If the method uses **input streaming** (Client side streaming or Bidirectional streaming), the generic function takes a single argument `(call)`. You can use `grpc:do-stream-receive` to cleanly iterate over incoming messages until EOF (`NIL`):

```lisp
(defmethod cl-protobufs.testing-rpc:say-hello-client-stream (call)
  (let (names)
    (grpc:do-stream-receive (req call)
      (push (cl-protobufs.testing:hello-request.name req) names))
    (cl-protobufs.testing:make-hello-reply :message (format nil "~{~A~^, ~}" (nreverse names)))))
```

If the method uses **output streaming** (Server side streaming or Bidirectional streaming), use `/server-send` (or `grpc:stream-send`) to send reply messages to the client. When your server method returns, gRPC automatically sends status OK and closes the call.

```lisp
(defmethod cl-protobufs.testing-rpc:say-hello-server-stream ((request cl-protobufs.testing:hello-request) call)
  (dotimes (i 3)
    (grpc:stream-send call (cl-protobufs.testing:make-hello-reply :message "Hello"))))
```

##### Advanced Server Error Handling

To abort a server streaming call early with a non-OK gRPC status code (e.g., `NOT_FOUND`, `INVALID_ARGUMENT`), use `grpc:abort-server-stream`:

```lisp
(grpc:abort-server-stream :grpc-status-not-found "Requested resource is unavailable")
```
gRPC will catch this condition and automatically transmit the specified status code and message to the client.

To start the server, use `run-grpc-proto-server`:

```lisp
(grpc:run-grpc-proto-server "localhost:8080" 'cl-protobufs.testing:greeter)
```

#### Example

This example can be found in examples/client/client-insecure.lisp.

## Further Reading

-   See https://grpc.io for more information on gRPC.
-   See examples/client/README.md for an example of how to run the example code.
-   For more on Cl-Protobufs read  https://github.com/qitab/cl-protobufs
