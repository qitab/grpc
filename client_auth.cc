// Copyright 2021 Google LLC
//
// Use of this source code is governed by an MIT-style
// license that can be found in the LICENSE file or at
// https://opensource.org/licenses/MIT.

#include <grpc/grpc_security.h>
#include <grpcpp/security/credentials.h>

namespace lisp {
namespace grpc {

extern "C" {

grpc_ssl_pem_key_cert_pair* create_grpc_ssl_pem_key_cert_pair(
    const char* private_key, const char* cert_chain) {
  // The validation check in the underlying cpp code will raise an error if either
  // of these is null and the overall struct isn't also a nullptr.
  // https://github.com/grpc/grpc/blob/master/src/core/lib/security/credentials/ssl/ssl_credentials.cc#L99-L101
  if (private_key == nullptr || cert_chain == nullptr) {
    return nullptr;
  }
  grpc_ssl_pem_key_cert_pair* keypair = new grpc_ssl_pem_key_cert_pair;
  *keypair = (grpc_ssl_pem_key_cert_pair) {
    .private_key = private_key,
    .cert_chain = cert_chain};
  return keypair;
}

void delete_grpc_ssl_pem_key_cert_pair(
    grpc_ssl_pem_key_cert_pair* keypair) {
  delete keypair;
}

grpc_ssl_verify_peer_options* create_grpc_ssl_verify_peer_options(
    int (*verify_peer_callback)(const char* target_name, const char* peer_pem, void* userdata),
    void* verify_peer_callback_userdata,
    void (*verify_peer_destruct)(void* userdata)) {
  grpc_ssl_verify_peer_options* options = new grpc_ssl_verify_peer_options;
  *options = (grpc_ssl_verify_peer_options) {
    .verify_peer_callback = verify_peer_callback,
    .verify_peer_callback_userdata = verify_peer_callback_userdata,
    .verify_peer_destruct = verify_peer_destruct};
  return options;
}

void delete_grpc_ssl_verify_peer_options(
    grpc_ssl_verify_peer_options* options) {
  delete options;
}

}  // extern "C"
}  // namespace grpc
}  // namespace lisp
