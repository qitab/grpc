#include <iostream>
#include <optional>
#include <string>

#include "net/grpc/public/include/grpc/grpc_security_google.h"
#include <grpc/grpc_security.h>
#include <grpc/grpc.h>
#include "net/grpc/public/include/grpcpp/server_credentials_google.h"
#include <grpc/grpc_security_constants.h>

namespace lisp {
namespace lisp_grpc {

extern "C" {
  
grpc_call_details* create_new_grpc_call_details() {
  grpc_call_details* arr = new grpc_call_details();
  grpc_call_details_init(arr);
  return arr;
}

bool grpc_server_call(
    grpc_server* server, grpc_call* call, grpc_call_details* details,
    grpc_metadata_array* request_metadata,
    grpc_completion_queue* cq_bound_to_call,
    grpc_completion_queue* cq_for_notification, void* tag) {
  grpc_call_error error = grpc_server_request_call(server, &call, details,
                                                   request_metadata,
                                                   cq_bound_to_call,
                                                   cq_for_notification,
                                                   tag);
  return (error == GRPC_CALL_OK);
}

grpc_server* start_server(grpc_completion_queue* cq) {
  // create the server
  grpc_server* server = grpc_server_create(nullptr, nullptr);
  std::string server_address = "localhost:8000";
  void* method;

  grpc_server_register_completion_queue(server, cq, nullptr);

  grpc_loas2_credentials_options server_creds_options =
      grpc_loas2_credentials_options{
    .desired_role = nullptr,
    .min_security_level =  GRPC_SECURITY_MIN,
    .serialized_server_authorization_policy = nullptr,
    .serialized_server_authorization_policy_length = 0,
    .instance_info_required = 0};
  grpc_server_credentials* server_creds = grpc_loas2_server_credentials_create(&server_creds_options);

  grpc_server_add_http2_port(server, server_address.c_str(), server_creds);

  method = grpc_server_register_method(server, "/xyz",
                                       const_cast<char*>(server_address.c_str()),
                                       GRPC_SRM_PAYLOAD_NONE, 0);

  grpc_server_start(server);
  grpc_server_credentials_release(server_creds);
  return server;
}

// Creates a grpc_call* given a 'channel', which manages the
}
}  // namespace lisp_grpc
}  // namespace lisp
