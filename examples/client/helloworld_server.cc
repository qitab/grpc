// Copyright 2021 Google LLC
//
// Use of this source code is governed by an MIT-style
// license that can be found in the LICENSE file or at
// https://opensource.org/licenses/MIT.

#include <fstream>
#include <iostream>
#include <memory>
#include <sstream>

#include <absl/flags/parse.h>
#include <absl/flags/flag.h>
#include <absl/strings/ascii.h>
#include <absl/strings/str_cat.h>
#include "third_party/absl/strings/string_view.h"
#include <grpc/grpc.h>
#include <grpcpp/security/server_credentials.h>
#include <grpcpp/server.h>
#include <grpcpp/server_builder.h>
#include <grpcpp/server_context.h>
#include "examples/client/helloworld.grpc.pb.h"

using grpc::InsecureServerCredentials;
using grpc::Server;
using grpc::ServerBuilder;
using grpc::ServerContext;
using grpc::ServerReader;
using grpc::ServerReaderWriter;
using grpc::ServerWriter;
using grpc::SslServerCredentials;
using grpc::SslServerCredentialsOptions;
using grpc::Status;
using lisp::grpc::integration_testing::HelloReply;
using lisp::grpc::integration_testing::HelloRequest;
using lisp::grpc::integration_testing::grpc_gen::Greeter;

ABSL_FLAG(int32, port, 0 , "Port server listening on.");
ABSL_FLAG(std::string, auth_mechanism, "", "Authentication mechanism.");
ABSL_FLAG(std::string, root_cert_path, "", "Path to root certificates.");
ABSL_FLAG(std::string, private_key_path, "", "Path to private key.");
ABSL_FLAG(std::string, certificate_chain_path, "",
          "Path to certificate chain.");

class GreeterServiceImpl final : public Greeter::Service {
  Status SayHello(ServerContext* context, const HelloRequest* request,
                  HelloReply* reply) override {
    reply->set_message(absl::StrCat("Hello ", request->name()));
    return Status::OK;
  }

  Status SayHelloServerStream(ServerContext* context,
                              const HelloRequest* request,
                              ServerWriter<HelloReply>* stream)
      override {
    HelloReply reply = HelloReply();
    for(int i = 0; i < request->num_responses(); i++) {
      reply.set_message(absl::StrCat("Hello ", request->name(), " ", i));
      stream->Write(reply);
    }
    return ::grpc::Status::OK;
  }

  Status SayHelloClientStream(ServerContext* context,
                              ServerReader<HelloRequest>* stream,
                              HelloReply* reply)
      override {
    std::string reply_string = "Hello ";
    HelloRequest request;
    while (stream->Read(&request)) {
      absl::StrAppend(&reply_string, request.name());
    }
    reply->set_message(reply_string);
    return ::grpc::Status::OK;
  }

  Status SayHelloBidirectionalStream(
      ServerContext* context,
      ServerReaderWriter<HelloReply, HelloRequest>* stream)
      override {
    HelloRequest request;
    while (stream->Read(&request)) {
      HelloReply reply = HelloReply();
      for(int i = 0; i < request.num_responses(); i++) {
        reply.set_message(absl::StrCat("Hello ", request.name(), " ", i));
        stream->Write(reply);
      }
    }
    return ::grpc::Status::OK;
  }

};

std::string readFileIntoString(absl::string_view path) {
  std::ifstream input_file(path);
  if (!input_file.is_open()) {
    std::cerr << "Could not open the file - '" << path << "'" << std::endl;
  }
  return std::string((std::istreambuf_iterator<char>(input_file)),
                     std::istreambuf_iterator<char>());
}

bool fileExists(const std::string& path) {
  std::ifstream f(path.c_str());
  return f.good();
}

void RunServer() {
  const int port = absl::GetFlag(FLAGS_port);
  QCHECK(port > 0) << "Please specify a valid server port";
  std::string server_address = absl::StrCat("localhost:", port);
  GreeterServiceImpl service;

  ServerBuilder builder;
  std::shared_ptr<grpc::ServerCredentials> creds;

  // Set up authentication mechanism (or lack therof) for the server.
  std::string auth_mechanism = absl::GetFlag(FLAGS_auth_mechanism);
  absl::AsciiStrToLower(&auth_mechanism);
  absl::RemoveExtraAsciiWhitespace(&auth_mechanism);
  if (auth_mechanism == "insecure") {
    creds = grpc::InsecureServerCredentials();
  } else if (auth_mechanism == "ssl") {
    const std::string root_cert_path = absl::GetFlag(FLAGS_root_cert_path);
    if (root_cert_path.empty() || !fileExists(root_cert_path)) {
      std::cout << "A valid root certificate must be specified, got: '"
                << root_cert_path << "'" << std::endl;
      return;
    }
    const std::string private_key_path = absl::GetFlag(FLAGS_private_key_path);
    if (private_key_path.empty() || !fileExists(private_key_path)) {
      std::cout << "A valid private key must be specified, got: '"
                << private_key_path << "'" << std::endl;
      return;
    }
    const std::string cert_chain_path =
        absl::GetFlag(FLAGS_certificate_chain_path);
    if (cert_chain_path.empty() || !fileExists(cert_chain_path)) {
      std::cout << "A valid certificate chain must be specified, got: '"
                << cert_chain_path << "'" << std::endl;
      return;
    }
    grpc::SslServerCredentialsOptions ssl_opts;
    ssl_opts.pem_root_certs = readFileIntoString(root_cert_path);
    grpc::SslServerCredentialsOptions::PemKeyCertPair pkcp = {
        readFileIntoString(private_key_path),
        readFileIntoString(cert_chain_path),
    };
    ssl_opts.pem_key_cert_pairs.push_back(pkcp);
    creds = grpc::SslServerCredentials(ssl_opts);
  } else {
    std::cout << "A valid authentication mechanism must be specified, got: '"
               << auth_mechanism << "'" << std::endl;
    return;
  }

  builder.AddListeningPort(server_address, creds);
  builder.RegisterService(&service);
  std::unique_ptr<Server> server(builder.BuildAndStart());
  std::cout << "Server listening on " << server_address << std::endl;
  server->Wait();
}

int main(int argc, char** argv) {
  absl::ParseCommandLine(argc, argv);
  RunServer();
  return 0;
}
