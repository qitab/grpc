# gRPC Client Example

## Insecure Example

The code in `client-insecure.lisp` shows how to make a gRPC client call over an insecure channel.

To run this example, first start the server:


TODO(b/199391296): Make the server externally usable.

Then run the client:

```sh
blaze run //third_party/lisp/grpc/testing/client:testing_insecure_client -- --port=50051 --logtostderr
```


