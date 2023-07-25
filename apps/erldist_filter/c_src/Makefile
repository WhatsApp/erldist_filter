# Copyright (c) Meta Platforms, Inc. and affiliates.
# Copyright (c) WhatsApp LLC
#
# This source code is licensed under the MIT license found in the
# LICENSE.md file in the root directory of this source tree.

# Based on c_src.mk from erlang.mk by Loic Hoguin <essen@ninenines.eu>

CURDIR := $(shell pwd)
BASEDIR := $(abspath $(CURDIR)/..)

PROJECT = erldist_filter

# Configuration.

C_SRC_DIR ?= $(CURDIR)
C_SRC_ENV ?= $(C_SRC_DIR)/env.mk
C_SRC_OUTPUT ?= $(BASEDIR)/priv/$(PROJECT)_nif
C_SRC_TYPE ?= shared

# "erl" command.

ERL_CMD ?= erl
ERL = $(ERL_CMD) +A1 -noinput -boot no_dot_erlang

# Platform detection.

ifeq ($(PLATFORM),)
UNAME_S := $(shell uname -s)

ifeq ($(UNAME_S),Linux)
PLATFORM = linux
else ifeq ($(UNAME_S),Darwin)
PLATFORM = darwin
else ifeq ($(UNAME_S),SunOS)
PLATFORM = solaris
else ifeq ($(UNAME_S),GNU)
PLATFORM = gnu
else ifeq ($(UNAME_S),FreeBSD)
PLATFORM = freebsd
else ifeq ($(UNAME_S),NetBSD)
PLATFORM = netbsd
else ifeq ($(UNAME_S),OpenBSD)
PLATFORM = openbsd
else ifeq ($(UNAME_S),DragonFly)
PLATFORM = dragonfly
else ifeq ($(shell uname -o),Msys)
PLATFORM = msys2
else
$(error Unable to detect platform. Please open a ticket with the output of uname -a.)
endif

export PLATFORM
endif

# System type and C compiler/flags.

ifeq ($(PLATFORM),msys2)
	C_SRC_OUTPUT_EXECUTABLE_EXTENSION ?= .exe
	C_SRC_OUTPUT_SHARED_EXTENSION ?= .dll
else
	C_SRC_OUTPUT_EXECUTABLE_EXTENSION ?=
	C_SRC_OUTPUT_SHARED_EXTENSION ?= .so
endif

ifeq ($(C_SRC_TYPE),shared)
	C_SRC_OUTPUT_FILE = $(C_SRC_OUTPUT)$(C_SRC_OUTPUT_SHARED_EXTENSION)
else
	C_SRC_OUTPUT_FILE = $(C_SRC_OUTPUT)$(C_SRC_OUTPUT_EXECUTABLE_EXTENSION)
endif

ifeq ($(PLATFORM),msys2)
# We hardcode the compiler used on MSYS2. The default CC=cc does
# not produce working code. The "gcc" MSYS2 package also doesn't.
	CC = /mingw64/bin/gcc
	export CC
	CXX = /mingw64/bin/g++
	export CXX
	CFLAGS ?= -O2 -g -ggdb -std=c2x -finline-functions -Wall -Wmissing-prototypes
	CXXFLAGS ?= -O2 -g -ggdb -std=c++20 -finline-functions -Wall
else ifeq ($(PLATFORM),darwin)
	CC ?= cc
	CXX ?= c++
	CFLAGS ?= -Og -g -ggdb -std=c2x -arch x86_64 -Wall -Wmissing-prototypes
	CXXFLAGS ?= -Og -g -ggdb -std=c++20 -arch x86_64 -Wall
	LDFLAGS ?= -arch x86_64 -flat_namespace -undefined suppress
else ifeq ($(PLATFORM),freebsd)
	CC ?= cc
	CXX ?= c++
	CFLAGS ?= -O2 -g -ggdb -std=c2x -finline-functions -Wall -Wmissing-prototypes
	CXXFLAGS ?= -O2 -g -ggdb -std=c++20 -finline-functions -Wall
else ifeq ($(PLATFORM),linux)
	CC ?= gcc
	CXX ?= g++
	CFLAGS ?= -O2 -g -ggdb -std=c2x -finline-functions -Wall -Wmissing-prototypes
	CXXFLAGS ?= -O2 -g -ggdb -std=c++20 -finline-functions -Wall
else ifeq ($(PLATFORM),solaris)
	CC ?= cc
	CXX ?= c++
	CFLAGS ?= -O2 -g -ggdb -std=c2x -finline-functions -fstack-protector -Wall -Wmissing-prototypes
	CXXFLAGS ?= -O2 -g -ggdb -std=c++20 -finline-functions -fstack-protector -Wall
endif

ifneq ($(PLATFORM),msys2)
	CFLAGS += -fPIC
	CXXFLAGS += -fPIC
endif

ifneq ($(CROSSCOMPILER),)
	CC = $(CROSSCOMPILER)gcc
	CXX = $(CROSSCOMPILER)g++
endif

CFLAGS += -I"$(ERTS_INCLUDE_DIR)" -I"$(ERL_INTERFACE_INCLUDE_DIR)" -fno-omit-frame-pointer
CXXFLAGS += -I"$(ERTS_INCLUDE_DIR)" -I"$(ERL_INTERFACE_INCLUDE_DIR)" -fno-omit-frame-pointer

LDLIBS += -L"$(ERL_INTERFACE_LIB_DIR)"

# Verbosity.

V ?= 0

verbose_0 = @
verbose_2 = set -x;
verbose = $(verbose_$(V))

ifeq ($(V),3)
SHELL := $(SHELL) -x
endif

gen_verbose_0 = @echo " GEN   " $@;
gen_verbose_2 = set -x;
gen_verbose = $(gen_verbose_$(V))

gen_verbose_esc_0 = @echo " GEN   " $$@;
gen_verbose_esc_2 = set -x;
gen_verbose_esc = $(gen_verbose_esc_$(V))

c_verbose_0 = @echo " C     " $(filter-out $(notdir $(MAKEFILE_LIST) $(C_SRC_ENV)),$(^F));
c_verbose = $(c_verbose_$(V))

cpp_verbose_0 = @echo " CPP   " $(filter-out $(notdir $(MAKEFILE_LIST) $(C_SRC_ENV)),$(^F));
cpp_verbose = $(cpp_verbose_$(V))

link_verbose_0 = @echo " LD    " $(@F);
link_verbose = $(link_verbose_$(V))

# Targets.

ifeq ($(PLATFORM),msys2)
core_native_path = $(shell cygpath -m $1)
else
core_native_path = $1
endif

# We skip files that contain spaces because they end up causing issues.
core_find = $(if $(wildcard $1),$(shell find $(1:%/=%) \( -type l -o -type f \) -name $(subst *,\*,$2) | grep -v " "))

ifeq ($(SOURCES),)
SOURCES := $(sort $(foreach pat,*.c *.C *.cc *.cpp,$(call core_find,$(C_SRC_DIR)/nif/,$(pat))))
endif
OBJECTS = $(addsuffix .o, $(basename $(SOURCES)))

ifeq ($(FORMAT_SOURCES),)
FORMAT_SOURCES := $(shell find $(C_SRC_DIR)/nif \( -type l -o -type f \) \( -name '*.c' -o -name '*.C' -o -name '*.cc' -o -name '*.cpp' -o -name '*.h' -o -name '*.c.h' -o -name '*.hpp' \) | grep -v " ")
endif

COMPILE_C = $(c_verbose) $(CC) $(CFLAGS) $(CPPFLAGS) -c
COMPILE_CPP = $(cpp_verbose) $(CXX) $(CXXFLAGS) $(CPPFLAGS) -c

all:: app

app:: $(C_SRC_ENV) $(C_SRC_OUTPUT_FILE)

test-build:: $(C_SRC_ENV) $(C_SRC_OUTPUT_FILE)

$(C_SRC_OUTPUT_FILE): $(OBJECTS)
	$(verbose) mkdir -p $(dir $@)
	$(link_verbose) $(CC) $(OBJECTS) \
		$(LDFLAGS) $(if $(filter $(C_SRC_TYPE),shared),-shared) $(LDLIBS) \
		-o $(C_SRC_OUTPUT_FILE)

$(OBJECTS): $(MAKEFILE_LIST) $(C_SRC_ENV)

%.o: %.c
	$(COMPILE_C) $(OUTPUT_OPTION) $<

%.o: %.cc
	$(COMPILE_CPP) $(OUTPUT_OPTION) $<

%.o: %.C
	$(COMPILE_CPP) $(OUTPUT_OPTION) $<

%.o: %.cpp
	$(COMPILE_CPP) $(OUTPUT_OPTION) $<

clean:: clean-c_src

clean-c_src:
	$(gen_verbose) rm -f $(C_SRC_OUTPUT_FILE) $(OBJECTS)

distclean:: distclean-c_src-env

distclean-c_src-env:
	$(gen_verbose) rm -f $(C_SRC_ENV)

format:
	$(gen_verbose) clang-format -i $(FORMAT_SOURCES)

ifneq ($(wildcard $(C_SRC_DIR)),)
ERL_ERTS_DIR = $(shell $(ERL) -eval 'io:format("~s~n", [code:lib_dir(erts)]), halt().')

$(C_SRC_ENV):
	$(verbose) $(ERL) -eval "file:write_file(\"$(call core_native_path,$(C_SRC_ENV))\", \
		io_lib:format( \
			\"# Generated by Erlang.mk. Edit at your own risk!~n~n\" \
			\"ERTS_INCLUDE_DIR ?= ~s/erts-~s/include/~n\" \
			\"ERL_INTERFACE_INCLUDE_DIR ?= ~s~n\" \
			\"ERL_INTERFACE_LIB_DIR ?= ~s~n\" \
			\"ERTS_DIR ?= $(ERL_ERTS_DIR)~n\" \
			\"ERTS_BIN_DIR ?= ~s/erts-~s/bin~n\", \
			[code:root_dir(), erlang:system_info(version), \
			code:lib_dir(erl_interface, include), \
			code:lib_dir(erl_interface, lib), \
			code:root_dir(), erlang:system_info(version)])), \
		halt()."

-include $(C_SRC_ENV)

ifneq ($(ERL_ERTS_DIR),$(ERTS_DIR))
$(shell rm -f $(C_SRC_ENV))
endif
endif
