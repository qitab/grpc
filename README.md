# gRPC Client Library in Common Lisp

```
* Owning team: http://g/common-lisp-dev
```

## Overview

This package defines a gRPC client library for Common Lisp. It wraps gRPC core
functions with CFFI calls and uses those core functions to create and use a
client. grpc-client.lisp contains all the necessary functions to create a gRPC
client by creating channels (connections between client and server) and calls
(requests to a server).

Currently there is support for synchronous calls over secure (LOAS2 only) and
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

#### LOAS2 Authentication

If using LOAS2 for authentication, create a channel with the `with-loas2-channel`
macro. This macro expects a symbol to bind the channel to, the server address,
and LOAS2 credential options.

```lisp
(with-loas2-channel (channel-bound-to-me ("localhost:8080" ()))
;; Code that uses channel
...)

;; () uses default LOAS2 credentials options
```

### Sending RPC Requests

Once a channel has been created, RPC requests to the server can occur using `grpc-call`.
This expects the channel that was previously created, the service name and method to be
called, and the request message serialized to bytes.

```lisp
(grpc:grpc-call channel
                "/serviceName/ServiceMethod"
                serialized-message)
;; Returns the response a list of byte vectors for each response
```

-   See https://grpc.io for more information on gRPC.
-   See testing/client/README.md for an example of how to run the example code.
