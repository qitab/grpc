#!/bin/bash
#

source gbash.sh || exit 1
source module gbash_unit.sh || exit 1

readonly HELLOWORLD_SERVER_PORT="$(reserve_random_unused_tcp_port)" || fail "Cannot find available port"
readonly HELLOWORLD_SERVER_ADDRESS="localhost:${HELLOWORLD_SERVER_PORT}"
readonly HELLOWORLD_SERVER="${TEST_SRCDIR}/google3/third_party/lisp/grpc/examples/client/helloworld_server"
readonly HELLOWORLD_CLIENT="${TEST_SRCDIR}/google3/third_party/lisp/grpc/examples/client/client-insecure"

function test::insecure::setup() {
  echo "Starting up helloworld server at ${HELLOWORLD_SERVER_ADDRESS}"

  # The RPC_COMMAND_TIMELIMIT and START_TIMELIMIT environment variables
  # sets the maximum time we start and to receive an RPC respectively.
  # The actual value was set empirically.
  START_TIMELIMIT=240 RPC_COMMAND_TIMELIMIT=120 start_google_server "${HELLOWORLD_SERVER_PORT}" \
    "${HELLOWORLD_SERVER}" --port ${HELLOWORLD_SERVER_PORT} --auth_mechanism="insecure"
  echo "helloworld server is running at ${HELLOWORLD_SERVER_ADDRESS}"
}

function test::insecure::teardown() {
  echo "Shutting down helloworld server at ${HELLOWORLD_SERVER_ADDRESS}"
  stop_google_server ${HELLOWORLD_SERVER_PORT}
  echo "helloworld server shut down"
}

function test::insecure::client_call() {
  ${HELLOWORLD_CLIENT} --port ${HELLOWORLD_SERVER_PORT} --logtostderr
  RESULT=$?
  EXPECT_EQ 0 $RESULT
}

gbash::unit::main "$@"

