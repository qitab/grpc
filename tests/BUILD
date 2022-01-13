# Description: Common Lisp gRPC library tests.
load("//lisp/devtools/bazel:rules.bzl", "lisp_library", "lisp_test")
load("//lisp/devtools/proto:clpb.bzl", "lisp_proto_library")

licenses(["notice"])

lisp_library(
    name = "root-suite",
    srcs = [
        "pkgdcl.lisp",
        "root-suite.lisp",
    ],
    deps = [
        "//third_party/lisp/clunit2",
    ],
)

proto_library(
    name = "test-proto",
    srcs = ["test.proto"],
    has_services = 1,
)

lisp_proto_library(
    name = "test-clpb",
    deps = [
        ":test-proto",
    ],
)

lisp_test(
    name = "protobuf-integration-test",
    size = "small",
    srcs = ["protobuf-integration-test.lisp"],
    main = "grpc.test.protobuf-integration:run",
    deps = [
        ":root-suite",
        ":test-clpb",
        "//third_party/lisp/grpc",
    ],
)
