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

lisp_test(
    name = "server-test",
    size = "small",
    srcs = ["server-test.lisp"],
    main = "grpc.test.server:run",
    deps = [
        ":root-suite",
        "//third_party/lisp/flexi-streams",
        "//third_party/lisp/grpc",
    ],
)

lisp_test(
    name = "integration-test",
    size = "small",
    srcs = ["integration-test.lisp"],
    main = "grpc.test.server:run",
    deps = [
        ":root-suite",
        "//lisp/log",
        "//third_party/lisp/bordeaux-threads",
        "//third_party/lisp/cffi",
        "//third_party/lisp/cffi:libffi",
        "//third_party/lisp/flexi-streams",
        "//third_party/lisp/grpc",
    ],
)
