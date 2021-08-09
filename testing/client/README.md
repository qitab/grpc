# gRPC Client Example

The code in client.lisp shows how to make a gRPC client call with a protocol
buffer and print the response.

To run this example client first run the server:

```
blaze run //net/grpc/examples/helloworld:greeter_server -- --port=50051 --alsologtostderr
```

Then run the client:

```
blaze run //third_party/lisp/grpc/testing/client:testing-client -- --port 50051 --logtostderr
```
