# gRPC Client Example

## Insecure Example

The code in `client-insecure.lisp` shows how to make a gRPC client call over an insecure channel.

To run this example, first start the server:

```sh
blaze run //third_party/lisp/grpc/examples/client:helloworld_server -- --port=50051 --auth_mechanism="insecure"
```

Then run the client:

```sh
blaze run //third_party/lisp/grpc/examples/client:client-insecure -- --port=50051 --logtostderr
```

<!-- BEGIN_INTERNAL -->
## LOAS2 Example

The code in `client-loas2.lisp` shows how to make a gRPC client call utilizing LOAS2 for
authentication.

To run this example, first start the server:

```sh
blaze run //third_party/lisp/grpc/examples/client:helloworld_server -- --port=50051 --auth_mechanism="loas2"
```

Then run the client:

```sh
blaze run //third_party/lisp/grpc/examples/client:client-loas2 -- --port 50051 --logtostderr
```
<!-- END_INTERNAL -->

## SSL Example

The code in `client-ssl.lisp` shows how to make a gRPC client call over a secure channel, using SSL
for authentication.

To run this example, first start the server:

```sh
blaze run //third_party/lisp/grpc/examples/client:helloworld_server -- --port=50051 --auth_mechanism="ssl" --root_cert_path=<Path to root certificate> --private_key_path=<Path to private key> --certificate_chain_path=<Path to certificate chain>
```

Then run the client:

```sh
blaze run //third_party/lisp/grpc/examples/client:client-ssl -- --port=50051 --logtostderr --root_cert_path=<Path to root certificate> --private_key_path=<Path to private key> --cert_chain_path=<Path to certificate chain>
```
