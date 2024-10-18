// Copyright 2021 Google LLC
//
// Use of this source code is governed by an MIT-style
// license that can be found in the LICENSE file or at
// https://opensource.org/licenses/MIT.

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <cstddef>

#include <grpc/byte_buffer.h>
#include <grpc/grpc.h>
#include <grpc/impl/grpc_types.h>
#include <grpc/impl/slice_type.h>
#include <grpc/slice.h>
#include <grpc/status.h>
#include <grpc/support/time.h>

namespace lisp {
namespace lisp_grpc {

// Creates a gpr_timespec equal to the 'time_s' in seconds.
// This gpr_timespec can then be used as a deadline for a grpc call.
//
// Initial implmentation pulled from grpc tests:
// https://github.com/grpc/grpc/blob/9a606ec6180812fbb2582646b1aeab1dfeb13475/test/core/test_util/test_config.cc#L78
gpr_timespec grpc_timeout_seconds_to_deadline(size_t time_s) {
  return gpr_time_add(
      gpr_now(GPR_CLOCK_MONOTONIC),
      gpr_time_from_millis(static_cast<size_t>(1e3) * time_s, GPR_TIMESPAN));
}

extern "C" {

// Creates a grpc_call* given a 'channel', which manages the
// connection to the server, and 'call_name' which will tell the server
// how to understand information from the call, and the 'cq'
// which will start the actual call and return if successful when
// grpc_completion_queue_pluck or grpc_completion_queue_next is called.
grpc_call* lisp_grpc_channel_create_call(grpc_channel* channel,
                                         const char* call_name,
                                         grpc_completion_queue* cq,
                                         size_t* deadline_seconds) {
  gpr_timespec send_deadline;
  if (deadline_seconds != nullptr) {
    send_deadline = grpc_timeout_seconds_to_deadline(*deadline_seconds);
  } else {
    send_deadline = gpr_inf_future(GPR_CLOCK_MONOTONIC);
  }

  return grpc_channel_create_call(
      channel, nullptr, GRPC_PROPAGATE_DEFAULTS, cq,
      grpc_slice_from_copied_string(call_name), nullptr,
      send_deadline, nullptr);
}

// Prepares ops for completion queue pluck/next
// and returns a grpc_call_error, and if successful a
// call is added to the completion queue.
// Upon success lisp_grpc_completion_queue_pluck must be called
// with tag.
grpc_call_error lisp_grpc_call_start_batch(grpc_call* call, const grpc_op* ops,
                                           size_t num_ops, void* tag) {
  return grpc_call_start_batch(call, ops, num_ops, tag, nullptr);
}

// Checks the completion queue 'cq' for an element associated with
// 'tag' and if successful will start all the operations that were prepared
// in lisp_grpc_call_start_batch.
// This function will return a grpc_event* which will be checked to see if
// the operations were successful.
bool lisp_grpc_completion_queue_pluck(grpc_completion_queue* cq, void* tag) {
  grpc_event event = grpc_completion_queue_pluck(
      cq, tag, gpr_inf_future(GPR_CLOCK_MONOTONIC), nullptr);
  return event.success != 0;
}

// Creates enough memory for tag
void* new_tag(int num) {
  return new int(num);
}

grpc_metadata_array* create_new_grpc_metadata_array() {
  grpc_metadata_array* arr = new grpc_metadata_array();
  grpc_metadata_array_init(arr);
  return arr;
}

void delete_grpc_metadata_array(grpc_metadata_array* metadata) {
  grpc_metadata_array_destroy(metadata);
  delete metadata;
}

// Allocates a grpc_op* pointer for 'num_ops' number of grpc_op.
// The ownership is passed to the creator.
grpc_op* create_new_grpc_ops(int num_ops) {
  return (grpc_op*)calloc(num_ops, sizeof(grpc_op));
}

// Frees all memory associated with ops.
void grpc_ops_free(grpc_op* ops, int size) {
  int i = 0;
  for (i = 0; i < size; i++) {
    if (ops[i].op == GRPC_OP_SEND_INITIAL_METADATA) {
      delete ops[i].data.send_initial_metadata.metadata;
    }
    if (ops[i].op == GRPC_OP_SEND_MESSAGE) {
      grpc_byte_buffer_destroy(ops[i].data.send_message.send_message);
    }
    if (ops[i].op == GRPC_OP_RECV_MESSAGE) {
      delete ops[i].data.recv_message.recv_message;
    }
    if (ops[i].op == GRPC_OP_RECV_STATUS_ON_CLIENT) {
      grpc_metadata_array_destroy(ops[i].data.recv_status_on_client.
                                  trailing_metadata);
    }
    delete ops[i].data.recv_status_on_client.status;
    delete ops[i].data.recv_status_on_client.status_details;
    if (ops[i].op == GRPC_OP_RECV_INITIAL_METADATA) {
      grpc_metadata_array_destroy(ops[i].data.recv_initial_metadata
                                      .recv_initial_metadata);

    }
    if (ops[i].op == GRPC_OP_SEND_INITIAL_METADATA) {
      free(ops[i].data.send_initial_metadata.metadata);
    }
    if (ops[i].op == GRPC_OP_SEND_STATUS_FROM_SERVER) {
      delete ops[i].data.send_status_from_server.trailing_metadata;
    }
    if (ops[i].op == GRPC_OP_RECV_CLOSE_ON_SERVER) {
      free(ops[i].data.recv_close_on_server.cancelled);
    }

  }
  free(ops);
}

// Takes in a preallocated grpc_op array.
// Stores the given metadata, flags, and count for the
// GRPC_OP_SEND_INITIAL_METADATA operation.
void lisp_grpc_make_send_metadata_op(grpc_op* op, int index,
                                     grpc_metadata* metadata,
                                     size_t count, uint32_t flags) {
  op[index].op = GRPC_OP_SEND_INITIAL_METADATA;
  op[index].data.send_initial_metadata.count = count;
  op[index].data.send_initial_metadata.metadata = metadata;
  op[index].flags = flags;
  op[index].reserved = nullptr;
}

// Takes in a preallocated grpc_op array.
// Stores the given request for the
// GRPC_OP_SEND_MESSAGE operation.
void lisp_grpc_make_send_message_op(grpc_op* op, int index,
                                     grpc_byte_buffer* request) {
  op[index].op = GRPC_OP_SEND_MESSAGE;
  op[index].data.send_message.send_message = request;
  op[index].reserved = nullptr;
}

// Takes in a preallocated grpc_op array.
// Stores the given response for the
// GRPC_OP_RECV_MESSAGE operation.
void lisp_grpc_make_recv_message_op(grpc_op* op, int index, int flags) {
  op[index].op = GRPC_OP_RECV_MESSAGE;
  op[index].data.recv_message.recv_message = new grpc_byte_buffer*;
  op[index].reserved = nullptr;
  op[index].flags = flags;
}

grpc_byte_buffer* lisp_grpc_op_recv_message(grpc_op* op, int index) {
  return op[index].data.recv_message.recv_message == nullptr
             ? nullptr
             : *op[index].data.recv_message.recv_message;
}

// Takes in a preallocated grpc_op array.
// Stores the given metadata for the
// GRPC_OP_RECV_INITIAL_METADATA operation.
void lisp_grpc_make_recv_metadata_op(grpc_op* op, int index) {
  op[index].op = GRPC_OP_RECV_INITIAL_METADATA;
  op[index].data.recv_initial_metadata.recv_initial_metadata =
      create_new_grpc_metadata_array();
  op[index].reserved = nullptr;
}

grpc_metadata_array* lisp_grpc_op_get_initial_metadata(grpc_op* ops, int index)
{
  return ops[index].data.recv_initial_metadata.recv_initial_metadata;
}


// Takes in a preallocated grpc_op array.
// Stores the given flags for the
// GRPC_OP_SEND_CLOSE_FROM_CLIENT operation.
void lisp_grpc_client_make_close_op(grpc_op* op, int index, uint32_t flags) {
  op[index].op = GRPC_OP_SEND_CLOSE_FROM_CLIENT;
  op[index].flags = flags;
  op[index].reserved = nullptr;
}

// Takes in a preallocated grpc_op array.
// Stores the given trailing_metadata, status, details, and flags for the
// GRPC_OP_RECV_STATUS_ON_CLIENT operation.
void lisp_grpc_client_make_recv_status_op(grpc_op* op, int index, int flags) {
  op[index].op = GRPC_OP_RECV_STATUS_ON_CLIENT;
  op[index].data.recv_status_on_client.trailing_metadata =
      create_new_grpc_metadata_array();
  op[index].data.recv_status_on_client.status = new grpc_status_code();
  op[index].data.recv_status_on_client.status_details = new grpc_slice();
  op[index].flags = flags;
  op[index].reserved = nullptr;
}

grpc_metadata_array* lisp_grpc_op_get_trailing_metadata(grpc_op* ops, int index)
{
  return ops[index].data.recv_status_on_client.trailing_metadata;
}

grpc_status_code lisp_grpc_op_get_status(grpc_op* ops, int index) {
  return *ops[index].data.recv_status_on_client.status;
}

grpc_slice* lisp_grpc_op_get_status_details(grpc_op* ops, int index) {
  return ops[index].data.recv_status_on_client.status_details;
}

// Takes in a preallocated grpc_op array.
// Stores the given trailing_metadata, metadata_count, status, and flags, for
// the GRPC_OP_SEND_STATUS_FROM_SERVER operation.
void lisp_grpc_server_make_send_status_op(grpc_op* op,
                                          int index,
                                          grpc_metadata* trailing_metadata,
                                          uint32_t metadata_count,
                                          grpc_status_code status,
                                          uint32_t flags) {
  op[index].op = GRPC_OP_SEND_STATUS_FROM_SERVER;
  op[index].data.send_status_from_server.trailing_metadata = trailing_metadata;
  op[index].data.send_status_from_server.status = status;
  op[index].data.send_status_from_server.trailing_metadata_count =
      metadata_count;
  op[index].flags = flags;
  op[index].reserved = nullptr;
}

// Takes in a preallocated grpc_op array.
// Stores the given metadata, cancelled and flags for the
// GRPC_OP_RECV_CLOSE_ON_SERVER operation.
void lisp_grpc_server_make_close_op(grpc_op* op, int index, int* cancelled,
                                    uint32_t flags ) {
  op[index].op = GRPC_OP_RECV_CLOSE_ON_SERVER;
  op[index].data.recv_close_on_server.cancelled = cancelled;
  op[index].flags = flags;
  op[index].reserved = nullptr;
}

// Takes in a preallocated grpc_op array.
// Stores the given metadata, flags, and count for the
// GRPC_OP_SEND_STATUS_FROM_SERVER operation.
void lisp_grpc_make_send_status_from_server_op(grpc_op* op,
                                               int index,
                                               grpc_metadata* trailing_metadata,
                                               uint32_t metadata_count,
                                               grpc_status_code status,
                                               uint32_t flags) {
  op[index].op = GRPC_OP_SEND_STATUS_FROM_SERVER;
  op[index].data.send_status_from_server.trailing_metadata = trailing_metadata;
  op[index].data.send_status_from_server.trailing_metadata_count =
      metadata_count;
  op[index].data.send_status_from_server.status = status;
  op[index].flags = flags;
  op[index].reserved = nullptr;
}

// Auxiliary Functions

// This takes a string str and converts it to a grpc_slice.
grpc_slice* convert_string_to_grpc_slice(const char* str) {
  grpc_slice* slice = new grpc_slice();
  *slice = grpc_slice_from_copied_string(str);
  return slice;
}

// This takes a grpc_slice 'slice' and converts it to a grpc_byte_buffer*
// that can be sent to the server.
grpc_byte_buffer* convert_grpc_slice_to_grpc_byte_buffer(grpc_slice* slice) {
  grpc_byte_buffer* ret = new grpc_byte_buffer();
  ret = grpc_raw_byte_buffer_create(slice, 1);
  return ret;
}

grpc_byte_buffer* create_empty_grpc_byte_buffer() {
  return new grpc_byte_buffer();
}

grpc_slice* create_empty_grpc_slice() {
  return new grpc_slice();
}

grpc_status_code* create_empty_grpc_status_code() {
  return (grpc_status_code*) calloc(1, sizeof(grpc_status_code));
}

grpc_slice* get_grpc_slice_from_grpc_byte_buffer(grpc_byte_buffer* buf,
                                                 int index) {
  return &(buf->data.raw.slice_buffer.slices[index]);
}

int grpc_byte_buffer_slice_buffer_count(grpc_byte_buffer* buf) {
  return buf->data.raw.slice_buffer.count;
}

char* convert_grpc_slice_to_string(grpc_slice* slice) {
  return grpc_slice_to_c_string(*slice);
}

void free_grpc_slice(grpc_slice* slice) {
  free(slice);
}

grpc_byte_buffer* convert_bytes_to_grpc_byte_buffer(char* buf, size_t len) {
  grpc_slice slice = grpc_slice_from_copied_buffer(buf, len);
  grpc_byte_buffer* ret = new grpc_byte_buffer();
  ret = grpc_raw_byte_buffer_create(&slice, 1);
  return ret;
}

char* convert_grpc_byte_buffer_to_bytes(grpc_byte_buffer* buf, int index) {
  grpc_slice slice = buf->data.raw.slice_buffer.slices[index];
  return grpc_slice_to_c_string(slice);
}

grpc_slice* convert_bytes_to_grpc_slice(char* buf, size_t len) {
  grpc_slice* slice = new grpc_slice();
  *slice = grpc_slice_from_copied_buffer(buf, len);
  return slice;
}

}
}  // namespace lisp_grpc
}  // namespace lisp
