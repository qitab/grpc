#include <memory>

#include "base/commandlineflags.h"
#include <third_party/absl/flags/parse.h>
#include "base/logging.h"
#include "base/logging_extensions.h"
#include "net/grpc/public/include/grpc/grpc.h"
#include "net/grpc/public/include/grpcpp/security/server_credentials.h"
#include "net/grpc/public/include/grpcpp/server.h"
#include "net/grpc/public/include/grpcpp/server_builder.h"
#include "net/grpc/public/include/grpcpp/server_context.h"
#include "net/grpc/public/include/grpcpp/server_credentials_google.h"
#include <absl/flags/flag.h>
#include <absl/strings/ascii.h>
#include <absl/strings/str_cat.h>
#include "testing/client/helloworld.grpc.pb.h"

using grpc::InsecureServerCredentials;
using grpc::Loas2ServerCredentials;
using grpc::Server;
using grpc::ServerBuilder;
using grpc::ServerContext;
using grpc::Status;
using testing::HelloReply;
using testing::HelloRequest;
using testing::grpc_gen::Greeter;

DEFINE_int32(port, 0, "Port server listening on.");
DEFINE_string(auth_mechanism, "", "Authentication mechanism.");

class GreeterServiceImpl final : public Greeter::Service {
  Status SayHello(ServerContext* context, const HelloRequest* request,
                  HelloReply* reply) override {
    reply->set_message(absl::StrCat("Hello ", request->name()));
    return Status::OK;
  }
};

void RunServer() {
  const int port = absl::GetFlag(FLAGS_port);
  QCHECK(port > 0) << "Please specify a valid server port";
  std::string server_address = absl::StrCat("0.0.0.0:", port);
  GreeterServiceImpl service;

  ServerBuilder builder;
  std::shared_ptr<grpc::ServerCredentials> creds;

  // Set up authentication mechanism (or lack therof) for the server.
  std::string auth_mechanism = absl::GetFlag(FLAGS_auth_mechanism);
  absl::AsciiStrToLower(&auth_mechanism);
  absl::RemoveExtraAsciiWhitespace(&auth_mechanism);
  if (auth_mechanism == "insecure") {
    creds = grpc::InsecureServerCredentials();
  } else if (auth_mechanism == "loas2") {
    creds = Loas2ServerCredentials(grpc::Loas2ServerCredentialsOptions());
  } else {
    LOG(ERROR) << "A valid authentication mechanism must be specified, got: '"
               << auth_mechanism << "'";
    return;
  }

  builder.AddListeningPort(server_address, creds);
  builder.RegisterService(&service);
  std::unique_ptr<Server> server(builder.BuildAndStart());
  LOG(INFO) << "Server listening on " << server_address;
  server->Wait();
}

int main(int argc, char** argv) {
  absl::ParseCommandLine(argc, argv);
  absl::SetFlag(&FLAGS_alsologtostderr, true);
  RunServer();
  return 0;
}
