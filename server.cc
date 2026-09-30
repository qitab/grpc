#include <string>

#include <grpc/grpc.h>
#include <grpc/impl/slice_type.h>
#include <grpc/slice.h>
#include <grpc/support/time.h>

namespace lisp {
namespace lisp_grpc {

extern "C" {
grpc_call_details* create_new_grpc_call_details() {
  grpc_call_details* details = new grpc_call_details();
  grpc_call_details_init(details);
  return details;
}

char* grpc_call_method(grpc_call_details* call_details) {
  return grpc_slice_to_c_string(call_details->method);
}

void delete_grpc_call_details(grpc_call_details* call_details) {
  grpc_call_details_destroy(call_details);
  delete call_details;
}

grpc_call* lisp_grpc_server_request_call(
    grpc_server* server, grpc_call_details* details,
    grpc_metadata_array* request_metadata,
    grpc_completion_queue* cq_bound_to_call,
    grpc_completion_queue* cq_for_notification,
    void* tag) {
  grpc_call* internal_call = nullptr;
  grpc_call_error error = grpc_server_request_call(server,
                                                   &internal_call,
                                                   details,
                                                   request_metadata,
                                                   cq_bound_to_call,
                                                   cq_for_notification,
                                                   tag);

  if (error != GRPC_CALL_OK) {
    return nullptr;
  }

  grpc_event event = grpc_completion_queue_pluck(
      cq_for_notification, tag, gpr_inf_future(GPR_CLOCK_MONOTONIC), nullptr);

  if (event.success == 0)
    return nullptr;
  return internal_call;
}

grpc_server* grpc_run_server(grpc_server* server,
                             grpc_server_credentials* server_creds){
  // Start a server and release a server credentials object
  grpc_server_start(server);
  grpc_server_credentials_release(server_creds);
  return server;
}

void* register_method(grpc_server* server, const char* method_name, const char* server_address){
    return grpc_server_register_method(server,
                                       const_cast<char*>(("/"+std::string(method_name)).c_str()),
                                       server_address,
                                       {}, 0);
}

void register_server_completion_queue(grpc_server* server,
                                      grpc_completion_queue* cq) {
  if (server == nullptr || cq == nullptr) return;
  grpc_server_register_completion_queue(server, cq, nullptr);
}

grpc_server* start_server(grpc_completion_queue* cq,
                          grpc_server_credentials* server_creds,
                          const char* server_address) {
  grpc_arg arg;
  arg.type = GRPC_ARG_INTEGER;
  arg.key = const_cast<char*>(GRPC_ARG_ALLOW_REUSEPORT);
  arg.value.integer = 0;
  grpc_channel_args channel_args = {1, &arg};

  // create the server
  grpc_server* server = grpc_server_create(&channel_args, nullptr);

  if (cq != nullptr) {
    grpc_server_register_completion_queue(server, cq, nullptr);
  }

  grpc_server_add_http2_port(server, server_address, server_creds);

  return server;
}

void shutdown_server(grpc_server* server, grpc_completion_queue* cq, void* tag) {
  if (server == nullptr) return;
  grpc_server_shutdown_and_notify(server, cq, tag);
  grpc_completion_queue_pluck(cq,
                              tag,
                              gpr_inf_future(GPR_CLOCK_MONOTONIC),
                              nullptr);
  grpc_server_destroy(server);
}

// Creates a grpc_call* given a 'channel', which manages the
}
}  // namespace lisp_grpc
}  // namespace lisp
