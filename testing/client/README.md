# gRPC Client Example

## Insecure Example

The code in `client-insecure.lisp` shows how to make a gRPC client call over an insecure channel.

To run this example, first start the server:

```sh
blaze run //third_party/lisp/grpc/testing/client:helloworld_server -- --port=50051 --auth_mechanism="insecure"
```

Then run the client:

```sh
blaze run //third_party/lisp/grpc/testing/client:testing_insecure_client -- --port=50051 --logtostderr
```

## LOAS2 Example

The code in `client-loas2.lisp` shows how to make a gRPC client call utilizing LOAS2 for
authentication.

To run this example, first start the server:

```sh
blaze run //third_party/lisp/grpc/testing/client:helloworld_server -- --port=50051 --auth_mechanism="loas2"
```

Then run the client:

```sh
blaze run //third_party/lisp/grpc/testing/client:testing_loas2_client -- --port 50051 --logtostderr
```
