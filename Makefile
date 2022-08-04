# Copyright 2021 Google LLC
#
# Use of this source code is governed by an MIT-style
# license that can be found in the LICENSE file or at
# https://opensource.org/licenses/MIT.

# This Makefile depends on the Google grpc library.

# Directory where Google's gRPC library is installed. This should be
# the same directory you gave as the --prefix option to ./configure
# when installing it.
GRPC_ROOT ?= /usr/local
LIBS = -lgrpc -lgpr -lgrpc++
CFLAGS = $(shell pkg-config grpc --cflags) -I$(GRPC_ROOT)/include
LDFLAGS = -L$(GRPC_ROOT)/lib $(LIBS)

# Default target if make is run with no arguments.
default_target: client.so

.PHONY : default_target

client.o: client.cc
	$(CXX) $(CFLAGS) -pthread -fPIC -c -o $@ $<

client.so: client.o
	$(CXX) -pthread -shared -Wl,--no-undefined -o $@ $(LDFLAGS) $<

clean:
	$(RM) client.o client.so
