#!/bin/bash
#

source gbash.sh || exit 1
source module gbash_unit.sh || exit 1

readonly HELLOWORLD_SERVER_PORT="$(reserve_random_unused_tcp_port)" || fail "Cannot find available port"
readonly HELLOWORLD_SERVER_ADDRESS="localhost:${HELLOWORLD_SERVER_PORT}"
readonly HELLOWORLD_SERVER="${TEST_SRCDIR}/google3/third_party/lisp/grpc/examples/client/helloworld_server"
readonly HELLOWORLD_CLIENT="${TEST_SRCDIR}/google3/third_party/lisp/grpc/examples/client/client-ssl"

readonly OPENSSL_CONF="[req]
  prompt = no
  distinguished_name = req_dn
  req_extensions = v3_req

  [ v3_req ]
  basicConstraints = CA:TRUE

  [ req_dn ]
  C = US
  CN = test_cert"

function test::ssl::setup() {
  mkdir -p ${TEST_TMPDIR}/certs
  openssl ecparam -genkey -name secp521r1 -out ${TEST_TMPDIR}/certs/privkey.pem
  openssl req -newkey rsa:2048 -nodes -keyout ${TEST_TMPDIR}/certs/server.key -x509 \
    -out ${TEST_TMPDIR}/certs/server.crt -subj "/C=CH/O=Test/OU=Server/CN=localhost" \
    -config <( echo "$OPENSSL_CONF" )
  openssl req -newkey rsa:2048 -nodes -keyout ${TEST_TMPDIR}/certs/client.key -x509 \
    -out ${TEST_TMPDIR}/certs/client.crt -subj "/C=CH/O=Test/OU=Server/CN=client" \
    -config <( echo "$OPENSSL_CONF" )

  echo "Starting up helloworld server at localhost:${HELLOWORLD_SERVER_PORT}"

  # The RPC_COMMAND_TIMELIMIT and START_TIMELIMIT environment variables
  # sets the maximum time we start and to receive an RPC respectively.
  # The actual value was set empirically.
  START_TIMELIMIT=240 RPC_COMMAND_TIMELIMIT=120 start_google_server "${HELLOWORLD_SERVER_PORT}" \
    "${HELLOWORLD_SERVER}" --port ${HELLOWORLD_SERVER_PORT} --auth_mechanism="ssl" \
    --private_key_path="${TEST_TMPDIR}/certs/server.key" \
    --certificate_chain_path="${TEST_TMPDIR}/certs/server.crt" \
    --root_cert_path="${TEST_TMPDIR}/certs/client.crt"
  echo "helloworld server is running at ${HELLOWORLD_SERVER_ADDRESS}"
}

function test::ssl::teardown() {
  stop_google_server ${HELLOWORLD_SERVER_PORT}
  rm -rf ${TEST_TMPDIR}/certs
}

function test::ssl::client_call() {
  ${HELLOWORLD_CLIENT} --port ${HELLOWORLD_SERVER_PORT} \
    --root_cert_path="${TEST_TMPDIR}/certs/server.crt" \
    --private_key_path="${TEST_TMPDIR}/certs/client.key" \
    --cert_chain_path="${TEST_TMPDIR}/certs/client.crt" \
    --logtostderr
  RESULT=$?
  EXPECT_EQ 0 $RESULT
}

gbash::unit::main "$@"

