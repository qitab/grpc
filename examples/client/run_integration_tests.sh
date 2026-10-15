#!/bin/bash
# Copyright 2026 Google LLC
#
# Use of this source code is governed by an MIT-style
# license that can be found in the LICENSE file or at
# https://opensource.org/licenses/MIT.

# Runs the Lisp example clients against the C++ helloworld_server, using both
# insecure and SSL channel credentials.
#
# This is the open-source counterpart of insecure_integration_test.sh and
# ssl_integration_test.sh (which depend on google3-only shell libraries). It is
# run by .github/workflows/ci.yml.
#
# Usage:
#   run_integration_tests.sh <path/to/helloworld_server> <path/to/quicklisp/setup.lisp>
#
# Requires sbcl, openssl, python3, and protoc + protoc-gen-cl-pb on PATH, with
# the grpc, cl-protobufs and ace.flag systems loadable through Quicklisp.

set -euo pipefail

if [[ $# -ne 2 ]]; then
  echo "Usage: $0 <path/to/helloworld_server> <path/to/quicklisp/setup.lisp>" >&2
  exit 2
fi

readonly SERVER="$1"
readonly QL_SETUP="$2"
WORK_DIR="$(mktemp -d)"
readonly WORK_DIR
SERVER_PID=""

cleanup() {
  if [[ -n "${SERVER_PID}" ]]; then
    kill "${SERVER_PID}" 2>/dev/null || true
    wait "${SERVER_PID}" 2>/dev/null || true
  fi
  rm -rf "${WORK_DIR}"
}
trap cleanup EXIT

pick_port() {
  python3 -c 'import socket; s = socket.socket(); s.bind(("localhost", 0)); print(s.getsockname()[1])'
}

# start_server <port> <server flag>...
start_server() {
  local port="$1"
  shift
  echo "Starting helloworld_server on localhost:${port}"
  "${SERVER}" --port="${port}" "$@" &
  SERVER_PID=$!
  for _ in $(seq 1 60); do
    if (echo > "/dev/tcp/localhost/${port}") 2>/dev/null; then
      return 0
    fi
    if ! kill -0 "${SERVER_PID}" 2>/dev/null; then
      echo "helloworld_server exited before it started listening" >&2
      SERVER_PID=""
      return 1
    fi
    sleep 1
  done
  echo "Timed out waiting for helloworld_server on localhost:${port}" >&2
  return 1
}

stop_server() {
  kill "${SERVER_PID}" 2>/dev/null || true
  wait "${SERVER_PID}" 2>/dev/null || true
  SERVER_PID=""
}

# run_client <asdf system> <lisp package> <client flag>...
# Any error in the client makes sbcl exit with a non-zero status.
run_client() {
  local system="$1"
  local package="$2"
  shift 2
  local args=""
  local arg
  for arg in "$@"; do
    args+=" \"${arg}\""
  done
  sbcl --noinform --non-interactive \
    --load "${QL_SETUP}" \
    --eval '(ql:register-local-projects)' \
    --eval "(ql:quickload :${system})" \
    --eval "(ace.flag:parse-command-line :args (list ${args}))" \
    --eval "(${package}:main)"
}

echo "=== Insecure channel ==="
port="$(pick_port)"
start_server "${port}" --auth_mechanism=insecure
run_client grpc-examples/insecure testing-client-insecure --port "${port}"
stop_server

echo "=== SSL channel ==="
readonly CERTS="${WORK_DIR}/certs"
mkdir -p "${CERTS}"
cat > "${WORK_DIR}/openssl.cnf" <<'EOF'
[req]
prompt = no
distinguished_name = req_dn
req_extensions = v3_req

[ v3_req ]
basicConstraints = CA:TRUE

[ req_dn ]
C = US
CN = test_cert
EOF
openssl req -newkey rsa:2048 -nodes -keyout "${CERTS}/server.key" -x509 \
  -out "${CERTS}/server.crt" -subj "/C=CH/O=Test/OU=Server/CN=localhost" \
  -config "${WORK_DIR}/openssl.cnf"
openssl req -newkey rsa:2048 -nodes -keyout "${CERTS}/client.key" -x509 \
  -out "${CERTS}/client.crt" -subj "/C=CH/O=Test/OU=Server/CN=client" \
  -config "${WORK_DIR}/openssl.cnf"

port="$(pick_port)"
start_server "${port}" --auth_mechanism=ssl \
  --private_key_path="${CERTS}/server.key" \
  --certificate_chain_path="${CERTS}/server.crt" \
  --root_cert_path="${CERTS}/client.crt"
run_client grpc-examples/ssl testing-client-ssl --port "${port}" \
  --root_cert_path "${CERTS}/server.crt" \
  --private_key_path "${CERTS}/client.key" \
  --cert_chain_path "${CERTS}/client.crt"
stop_server

echo "All example integration tests passed."
