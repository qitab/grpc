// Copyright 2021 Google LLC
//
// Use of this source code is governed by an MIT-style
// license that can be found in the LICENSE file or at
// https://opensource.org/licenses/MIT.

#include "net/grpc/public/include/grpc/grpc_security_google.h"

namespace lisp {
namespace grpc {

extern "C" {

grpc_loas2_credentials_options* create_grpc_loas2_credentials_options(
    const char* desired_role, grpc_security_level min_security_level,
    const char* serialized_server_authorization_policy,
    size_t serialized_server_authorization_policy_length,
    int instance_info_required) {
  grpc_loas2_credentials_options* options = new grpc_loas2_credentials_options;
  *options = (grpc_loas2_credentials_options) {
      .desired_role = desired_role,
      .min_security_level = min_security_level,
      .serialized_server_authorization_policy =
      serialized_server_authorization_policy,
      .serialized_server_authorization_policy_length =
      serialized_server_authorization_policy_length,
      .instance_info_required = instance_info_required};
  return options;
}

void delete_grpc_loas2_credentials_options(
    grpc_loas2_credentials_options* options) {
  delete options;
}

}
}  // namespace grpc
}  // namespace lisp
