# Copyright (c) Meta Platforms, Inc. and affiliates.
# Copyright (c) WhatsApp LLC
#
# This source code is licensed under the MIT license found in the
# LICENSE.md file in the root directory of this source tree.

"""Generate C and Erlang code from YAML configuration using Jinja2."""
from .code_generator import CodeGenerator
from .schema import Root

__all__ = ["CodeGenerator", "Root"]
