# gRPC Client Library in Common Lisp

```
* Owning team: http://g/common-lisp-dev
```

## Overview

This package defines a gRPC client library for Common Lisp. It wraps gRPC core
functions with CFFI calls and uses those core functions to create and use a
client. grpc-client.lisp contains all the necessary functions to create a gRPC
client by creating channels (connections between client and server) and calls
(requests to a server). Currently there is support for synchronous calls over
LOAS2 channels. Support for asynchronous calls and other channel types will be
added in the future.

## Usage

To create a client first create a channel with the `with-loas2-channel` given a
symbol for the channel to be bound too, the address, and LOAS2 credentials
options.

```lisp
(with-loas2-channel (channel-bound-to-me ("localhost:8080" ()))
;; Code that uses channel
...)

;; () uses default LOAS2 credentials options
```

Then create a call and get the response with `grpc-call` given the channel, the
method name including the service, and the bytes of the request.

```lisp
(grpc:grpc-call channel "/serviceName/ServiceMethod"
                                          serialized-message)
;; Returns the response a list of byte vectors for each response
```

-   See https://grpc.io for more information on gRPC.
-   See testing/client/README.md for an example of how to run the example code.
