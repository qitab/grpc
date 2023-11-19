# gRPC Library in Common Lisp

[![SBCL-Tests](https://github.com/qitab/grpc/actions/workflows/SBCL-test.yml/badge.svg)](https://github.com/qitab/grpc/actions/workflows/SBCL-test.yml)
[![CCL-Tests](https://github.com/qitab/grpc/actions/workflows/CCL-test.yml/badge.svg)](https://github.com/qitab/grpc/actions/workflows/CCL-test.yml)



## Overview

This package defines a [gRPC](https://grpc.io/) library for Common Lisp.
It wraps gRPC core functions with CFFI calls.

``client.lisp`` contains all the necessary functions to
create a gRPC client by creating channels (connections between client and
server) and calls (requests to a server).
Currently there is support for synchronous and streaming calls over
SSL, and insecure channels.

``server.lisp`` contains a ``run-grpc-server`` function that implements an insecure gRPC server.

If your service is defined by a protobuf, ``protobuf-integration.lisp`` contains a ``run-grpc-proto-server`` function that serves a gRPC/protobuf service.


## Installation

1. Install the gRPC library for your platform (e.g. via a package manager).

2. Clone this repository into [a location findable by ASDF](https://asdf.common-lisp.dev/asdf.html#Configuring-ASDF-to-find-your-systems).

3. In this directory, build the library stub by running ``make``. You may need to make adjustments to the *Makefile* for your platform.

4. Load and use this system in your application, e.g. at a REPL with ``(asdf:load-system "grpc")`` or via a ``defsystem``.

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

```
(testing-rpc:say-hello-bidirectional-stream/start channel)
```

Takes in a `channel` object and returns a `call` object that the user must keep
until the call is `closed` and `cleanup` is called.

```
(testing-rpc:say-hello-bidirectional-stream/send call message)
```

Takes in the `call` object and a `message` and sends a message to the client.

```
(testing-rpc:say-hello-server-stream/receive call)
```

Blocks until a message is received, then returns that message.
`NIL` will be returned if the server closes the channel.

```
(testing-rpc:say-hello-server-stream/close call)
```

Will `close` the channel on the client side.

```
(testing-rpc:say-hello-server-stream/cleanup call)
```

Will safely cleanup any data leftover in the `call` object.

#### Example

This example can be found in examples/client/client-insecure.lisp.

### Server

A minimal gRPC service with protobuf integration looks like:

```
; load the protobuf-generated code here

(defmethod cl-protobufs.testing-rpc::say-hello ((request cl-protobufs.testing:hello-request) call)
  (make-hello-reply :name "world")

(defun main ()
  (grpc:init-grpc)
  (grpc::run-grpc-proto-server "localhost:8080" 'cl-protobufs.testing:greeter))
```

## Further Reading

-   See https://grpc.io for more information on gRPC.
-   See examples/client/README.md for an example of how to run the example code.
-   For more on Cl-Protobufs read  https://github.com/qitab/cl-protobufs
