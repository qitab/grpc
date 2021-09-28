# gRPC Client Library in Common Lisp



## Overview

This package defines a gRPC client library for Common Lisp. It wraps gRPC core
functions with CFFI calls and uses those core functions to create and use a
client. client.lisp contains all the necessary functions to create a gRPC
client by creating channels (connections between client and server) and calls
(requests to a server).

Currently there is support for synchronous calls over

insecure channels. Support for asynchronous calls and other channel types will
be added in the future.

## Usage

To create a client, a channel must first be created. Depending on the expected
authentication mechanism (or lack thereof), different channel creation macros
are available.

### Channel Creation

#### Insecure Channels

If using an insecure channel, use the `with-insecure-channel` macro. This macro
expects a symbol to bind the channel to and the server address.

```lisp
(with-insecure-channel (channel-bound-to-me "localhost:8080")
;; Code that uses channel
...)
```



### Sending RPC Requests

Once a channel has been created, RPC requests to the server can occur using `grpc-call`.
This expects the channel that was previously created, the service name and method to be
called, and the request message serialized to bytes.

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

Currently only unary and server streaming RPC's are supported.

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

#### Biderectional Streaming RPC

A bidirectional streaming RPC sends any number of messages and receives any number of messages.
The `grpc-call` function takes in a list of vectors for `bytes-to-send`
and returns a list of octet-vectors corresponding to the received messages.

### cl-protobufs Integration

gRPC can integrate with cl-protobufs to send Protocol Buffer messages
over gRPC. To use gRPC you must load cl-protobufs and gRPC with
grpc-protobuf-integration.lisp into your running lisp image.

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
  // This is a test method to receive a HelloRequest and send a HelloReply.
  rpc SayHello(HelloRequest) returns (HelloReply) {}
  // This method receives a HelloRequest requesting some number of responses in num_responses
  // and response with a HelloReply num_responses times.
  rpc SayHelloServerStream(HelloRequest) returns (stream HelloReply) {}
}
```

We create two packages:

* `cl-protobufs.testing`
* `cl-protobufs.testing-rpc`

The package `cl-protobufs.testing` contains the `hello-request` and `hello-reply`  protocol
buffer messages.

The package `cl-protobufs.testing-rpc` contains a stub for `call-say-hello`. A message can be
sent to a server implementing the `Greeter` service with:

```lisp
  (grpc:with-insecure-channel
      (channel (concatenate 'string hostname ":" (write-to-string port-number)))
    (let* ((request (cl-protobufs.testing:make-hello-request :name "Neo"))
           (response (cl-protobufs.testing-rpc:call-say-hello channel message)))
      ...))
```

This example can be found in testing/client/client-insecure.lisp.

## Further Reading

-   See https://grpc.io for more information on gRPC.
-   See testing/client/README.md for an example of how to run the example code.
-   For more on Cl-Protobufs read  https://github.com/qitab/cl-protobufs
