# Copyright (c) Meta Platforms, Inc. and affiliates.
# Copyright (c) WhatsApp LLC
#
# This source code is licensed under the MIT license found in the
# LICENSE.md file in the root directory of this source tree.

"""Context Channel models for codegen."""
import bisect
from abc import abstractclassmethod
from collections import OrderedDict
from dataclasses import dataclass, field
from typing import Any, Iterator, Optional, Union

from ..schema import (
    Channel as ChannelSchema,
    ChannelStat as ChannelStatSchema,
    ChannelStatDop as ChannelStatDopSchema,
)
from . import Atom, Context

ChannelStatField = Union["ChannelStat", "ChannelStatDop"]


@dataclass
class Channel:
    ctx: Context = field(repr=False)
    schema: ChannelSchema = field(repr=False)
    stats_dict: OrderedDict[str, ChannelStatField] = field(init=False)
    stats_list: list[ChannelStatField] = field(init=False)

    def __post_init__(self) -> None:
        self.stats_dict = OrderedDict()
        self.stats_list = []

        for stat_schema in self.schema.stats:
            if stat_schema.key in self.stats_dict:
                raise ValueError(f"Duplicate ChannelStat key: {stat_schema.key}")
            stat = ChannelStat(ctx=self.ctx, schema=stat_schema)
            self.stats_dict[stat.key] = stat
            self.stats_list.append(stat)

        for stat_dop_schema in self.schema.stats_dop:
            if stat_dop_schema.key in self.stats_dict:
                raise ValueError(f"Duplicate ChannelStatDop key: {stat_dop_schema.key}")
            stat_dop = ChannelStatDop(ctx=self.ctx, schema=stat_dop_schema)
            self.stats_dict[stat_dop.key] = stat_dop
            self.stats_list.append(stat_dop)


@dataclass
class ChannelStat:
    ctx: Context = field(repr=False)
    schema: ChannelStatSchema = field(repr=False)

    @property
    def key(self) -> str:
        return self.schema.key

    @property
    def c_kind(self) -> str:
        return "uint64_t"

    @property
    def erlang_kind(self) -> str:
        return "non_neg_integer()"


@dataclass
class ChannelStatDop:
    ctx: Context = field(repr=False)
    schema: ChannelStatDopSchema = field(repr=False)

    @property
    def key(self) -> str:
        return self.schema.key

    @property
    def c_kind(self) -> str:
        return "edf_channel_stats_dop_t"

    @property
    def erlang_kind(self) -> str:
        return "dop_stats()"
