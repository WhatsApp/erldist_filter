# Copyright (c) Meta Platforms, Inc. and affiliates.
# Copyright (c) WhatsApp LLC
#
# This source code is licensed under the MIT license found in the
# LICENSE.md file in the root directory of this source tree.

"""Context Test models for codegen."""
import bisect
from abc import abstractclassmethod
from collections import OrderedDict
from dataclasses import dataclass, field
import re
from typing import Any, Iterator, Optional, Union

from ..schema import (
    Dpi as DpiSchema,
)
from . import Atom, Context


@dataclass
class Dpi:
    ctx: Context = field(repr=False)
    schema: DpiSchema = field(repr=False)
    otp_name_blockdict: OrderedDict[str, "DpiOtpNameBlock"] = field(init=False)
    otp_name_blocklist: list["DpiOtpNameBlock"] = field(init=False)

    def __post_init__(self) -> None:
        self.otp_name_blockdict = OrderedDict()
        self.otp_name_blocklist = []

        for otp_name_block in self.schema.otp_name_blocklist:
            if otp_name_block in self.otp_name_blockdict:
                raise ValueError(f"Duplicate DpiOtpNameBlock key: {otp_name_block}")
            child: "DpiOtpNameBlock" = DpiOtpNameBlock(ctx=self.ctx, schema=otp_name_block)
            self.otp_name_blockdict[child.key] = child
            self.otp_name_blocklist.append(child)


@dataclass
class DpiOtpNameBlock:
    ctx: Context = field(repr=False)
    schema: str = field(repr=True)

    @property
    def key(self) -> str:
        return self.schema
