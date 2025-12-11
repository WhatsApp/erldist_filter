# Copyright (c) Meta Platforms, Inc. and affiliates.
# Copyright (c) WhatsApp LLC
#
# This source code is licensed under the MIT license found in the
# LICENSE.md file in the root directory of this source tree.

PROJECT = erldist_filter
PROJECT_DESCRIPTION = erldist_filter
PROJECT_VERSION = 1.28.3

include erlang.mk

.PHONY: eqwalize eqwalize-all distclean-elp

# Arch detection.

ifeq ($(ARCH),)
UNAME_M := $(shell uname -m)

ifeq ($(UNAME_M),amd64)
ARCH = x86_64
else ifeq ($(UNAME_M),x86_64)
ARCH = x86_64
else ifeq ($(UNAME_M),arm64)
ARCH = aarch64
else ifeq ($(UNAME_M),aarch64)
ARCH = aarch64
else
$(error Unable to detect architecture. Please open a ticket with the output of uname -s.)
endif

export ARCH
endif

# Configuration.
ELP_VERSION ?= 2025-12-11
ELP_OTP_VERSION ?= 28

ELP ?= $(CURDIR)/elp
export ELP

ifeq ($(PLATFORM),darwin)
	ELP_URL ?= https://github.com/WhatsApp/erlang-language-platform/releases/download/${ELP_VERSION}/elp-macos-${ARCH}-apple-darwin-otp-${ELP_OTP_VERSION}.tar.gz
else
	ELP_URL ?= https://github.com/WhatsApp/erlang-language-platform/releases/download/${ELP_VERSION}/elp-linux-${ARCH}-unknown-linux-gnu-otp-${ELP_OTP_VERSION}.tar.gz
endif

ELP_OPTS ?=
ELP_BUILD_DIR ?= $(CURDIR)/_elp_build
ELP_ARCHIVE = elp-$(ELP_VERSION).tar.gz

# Core targets.

help::
	$(verbose) printf "%s\n" "" \
		"elp targets:" \
		"  eqwalize     Run 'elp eqwalize-app argo' on the current project" \
		"  eqwalize-all Run 'elp eqwalize-all' on the current project"

distclean:: distclean-elp

# Plugin-specific targets.

$(ELP):
	$(verbose) mkdir -p $(ELP_BUILD_DIR)
	$(verbose) echo "Downloading eqwalizer from: "$(ELP_URL)
	$(verbose) $(call core_http_get,$(ELP_BUILD_DIR)/$(ELP_ARCHIVE),$(ELP_URL))
	$(verbose) cd $(ELP_BUILD_DIR) && \
		tar -xzf $(ELP_ARCHIVE)
	$(gen_verbose) cp $(ELP_BUILD_DIR)/elp $(ELP)
	$(verbose) chmod +x $(ELP)
	$(verbose) rm -rf $(ELP_BUILD_DIR)

eqwalize: $(ELP)
	$(verbose) $(ELP) eqwalize $(PROJECT)

eqwalize-all: $(ELP)
	$(verbose) $(ELP) eqwalize-all

distclean-elp:
	$(gen_verbose) rm -rf $(ELP)

.PHONY: erlfmt erlfmt-check distclean-erlfmt format

# Configuration.
ERLFMT_VERSION ?= 1.7.0

ERLFMT ?= $(CURDIR)/erlfmt
export ERLFMT

ERLFMT_URL ?= https://github.com/WhatsApp/erlfmt/archive/refs/tags/v$(ERLFMT_VERSION).tar.gz
ERLFMT_OPTS ?=
ERLFMT_BUILD_DIR ?= $(CURDIR)/_erlfmt_build
ERLFMT_CODE_ARCHIVE = $(ERLFMT_VERSION).tar.gz

ERLFMT_REBAR3_URL ?= https://s3.amazonaws.com/rebar3/rebar3
ERLFMT_REBAR3 ?= rebar3

# Core targets.

help::
	$(verbose) printf "%s\n" "" \
		"erlfmt targets:" \
		"  erlfmt       Run erlfmt or download the default otherwise" \
		"  elrfmt-check Run erlfmt --check"

distclean:: distclean-erlfmt

# Plugin-specific targets.

$(ERLFMT):
	$(verbose) mkdir -p $(ERLFMT_BUILD_DIR)
ifeq ($(shell command -v $(ERLFMT_REBAR3)),)
	$(verbose) echo "Downloading Rebar3 from: "$(ERLFMT_REBAR3_URL)
	$(verbose) $(call core_http_get,$(ERLFMT_BUILD_DIR)/rebar3,$(ERLFMT_REBAR3_URL))
	$(verbose) chmod +x $(ERLFMT_BUILD_DIR)/rebar3
	$(eval ERLFMT_REBAR3 := $(ERLFMT_BUILD_DIR)/rebar3)
else
	$(verbose) echo "Using Rebar3: "$(ERLFMT_REBAR3)
endif
	$(verbose) echo "Downloading erlfmt from: "$(ERLFMT_URL)
	$(verbose) $(call core_http_get,$(ERLFMT_BUILD_DIR)/$(ERLFMT_CODE_ARCHIVE),$(ERLFMT_URL))
	$(verbose) cd $(ERLFMT_BUILD_DIR) && \
		tar -xzf $(ERLFMT_CODE_ARCHIVE) && \
		cd erlfmt-$(ERLFMT_VERSION) && \
		$(ERLFMT_REBAR3) as release escriptize
	$(gen_verbose) cp $(ERLFMT_BUILD_DIR)/erlfmt-$(ERLFMT_VERSION)/_build/release/bin/erlfmt $(ERLFMT)
	$(verbose) chmod +x $(ERLFMT)
	$(verbose) rm -rf $(ERLFMT_BUILD_DIR)/erlfmt-$(ERLFMT_VERSION)
	$(verbose) rm $(ERLFMT_BUILD_DIR)/$(ERLFMT_CODE_ARCHIVE)
	$(verbose) rm -f $(ERLFMT_BUILD_DIR)/rebar3
	$(verbose) rm -rf $(ERLFMT_BUILD_DIR)

erlfmt: $(ERLFMT)
	$(verbose) $(ERLFMT) --verbose --write --require-pragma --print-width=120 \
		'apps/**/{src,include,test}/**/*.{hrl,erl,app.src}' \
		'apps/**/{rebar.config,rebar.config.script}' \
		'apps/**/test/**/*.config' \
		'{rebar.config,rebar.config.script}'

erlfmt-check: $(ERLFMT)
	$(verbose) $(ERLFMT) --check --require-pragma --print-width=120 \
		'apps/**/{src,include,test}/**/*.{hrl,erl,app.src,app.src.script}' \
		'apps/**/{rebar.config,rebar.config.script}' \
		'apps/**/test/**/*.config' \
		'{rebar.config,rebar.config.script}'

distclean-erlfmt:
	$(gen_verbose) rm -rf $(ERLFMT)

format: $(ERLFMT)
	$(verbose) $(MAKE) -C $(CURDIR)/apps/$(PROJECT)/c_src format
	$(verbose) $(MAKE) erlfmt
	$(verbose) mix format --migrate

.PHONY: lint lint-dialyzer lint-eqwalizer lint-format lint-xref

lint:: lint-format lint-eqwalizer lint-xref lint-dialyzer

lint-dialyzer:
	$(verbose) rebar3 dialyzer

lint-eqwalizer: eqwalize-all

lint-format: erlfmt-check
	$(verbose) $(MAKE) -C $(CURDIR)/apps/$(PROJECT)/c_src lint-format

lint-xref:
	$(verbose) rebar3 xref
