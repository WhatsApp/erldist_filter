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
    Test as TestSchema,
    TestConfig as TestConfigSchema,
    TestConfigDpi as TestConfigDpiSchema,
    TestConfigDpiPacket as TestConfigDpiPacketSchema,
    TestConfigHeader as TestConfigHeaderSchema,
    TestConfigHeaderMode as TestConfigHeaderModeSchema,
    TestConfigTraffic as TestConfigTrafficSchema,
)
from . import Atom, Context


@dataclass
class Test:
    ctx: Context = field(repr=False)
    schema: TestSchema = field(repr=False)
    config: "TestConfig" = field(init=False)

    def __post_init__(self) -> None:
        self.config = TestConfig(ctx=self.ctx, schema=self.schema.config)


@dataclass
class TestConfig:
    ctx: Context = field(repr=False)
    schema: TestConfigSchema = field(repr=False)
    dpi_dict: OrderedDict[str, "TestConfigDpi"] = field(init=False)
    dpi_list: list["TestConfigDpi"] = field(init=False)
    headers_dict: OrderedDict[str, "TestConfigHeader"] = field(init=False)
    headers_list: list["TestConfigHeader"] = field(init=False)
    traffic_dict: OrderedDict[str, "TestConfigTraffic"] = field(init=False)
    traffic_list: list["TestConfigTraffic"] = field(init=False)

    def __post_init__(self) -> None:
        self.dpi_dict = OrderedDict()
        self.dpi_list = []

        for dpi_schema in self.schema.dpi:
            if dpi_schema.key in self.dpi_dict:
                raise ValueError(f"Duplicate TestConfigDpi key: {dpi_schema.key}")
            dpi = TestConfigDpi(ctx=self.ctx, schema=dpi_schema)
            self.dpi_dict[dpi.key] = dpi
            self.dpi_list.append(dpi)

        self.headers_dict = OrderedDict()
        self.headers_list = []

        for header_schema in self.schema.headers:
            if header_schema.key in self.headers_dict:
                raise ValueError(f"Duplicate TestConfigHeader key: {header_schema.key}")
            header = TestConfigHeader(ctx=self.ctx, schema=header_schema)
            self.headers_dict[header.key] = header
            self.headers_list.append(header)

        self.traffic_dict = OrderedDict()
        self.traffic_list = []

        for traffic_schema in self.schema.traffic:
            if traffic_schema.key in self.traffic_dict:
                raise ValueError(f"Duplicate TestConfigTraffic key: {traffic_schema.key}")
            traffic = TestConfigTraffic(ctx=self.ctx, schema=traffic_schema)
            self.traffic_dict[traffic.key] = traffic
            self.traffic_list.append(traffic)


@dataclass
class TestConfigDpi:
    ctx: Context = field(repr=False)
    schema: TestConfigDpiSchema = field(repr=False)
    packets_dict: OrderedDict[str, "TestConfigDpiPacket"] = field(init=False)
    packets_keep: list["TestConfigDpiPacket"] = field(init=False)
    packets_list: list["TestConfigDpiPacket"] = field(init=False)

    def __post_init__(self) -> None:
        self.packets_dict = OrderedDict()
        self.packets_keep = []
        self.packets_list = []

        for packet_schema in self.schema.packets:
            if packet_schema.call in self.packets_dict:
                raise ValueError(f"Duplicate TestConfigDpiPacket call: {packet_schema.call}")
            packet = TestConfigDpiPacket(ctx=self.ctx, schema=packet_schema, index=len(self.packets_list))
            self.packets_dict[packet.call] = packet
            if not packet.drop and packet.log:
                self.packets_keep.append(packet)
            self.packets_list.append(packet)

    @property
    def key(self) -> str:
        return self.schema.key

    @property
    def has_setup(self) -> bool:
        return self.schema.setup is not None

    @property
    def setup(self) -> str | None:
        if self.has_setup:
            return self.schema.setup.strip()
        else:
            return None

    @property
    def drop_count(self) -> int:
        x: int = 0
        for packet in self.packets_list:
            if packet.drop:
                x += 1
        return x

    @property
    def logger_capacity(self) -> int:
        x: int = 0
        for packet in self.packets_list:
            if packet.drop or not packet.log:
                continue
            x += 1
        return x


@dataclass
class TestConfigDpiPacket:
    ctx: Context = field(repr=False)
    schema: TestConfigDpiPacketSchema = field(repr=False)
    index: int

    @property
    def call(self) -> str:
        return self.schema.call

    @property
    def drop(self) -> bool:
        return self.schema.drop

    @property
    def log(self) -> bool:
        return self.schema.log

    @property
    def has_payload(self) -> bool:
        return self.schema.payload is not None

    @property
    def payload(self) -> str | None:
        return self.schema.payload


@dataclass
class TestConfigHeader:
    ctx: Context = field(repr=False)
    schema: TestConfigHeaderSchema = field(repr=False)

    @property
    def key(self) -> str:
        return self.schema.key

    @property
    def label(self) -> str:
        return self.schema.label

    @property
    def mode(self) -> str:
        return self.ctx.erlang_atom(self.schema.mode.value)

    @property
    def fragment_size(self) -> int | None:
        return self.schema.fragment_size

    @property
    def dflags(self) -> str:
        if self.schema.mode == TestConfigHeaderModeSchema.pass_through:
            return self.ctx.erlang_atom("DFLAG_DIST_MANDATORY")
        else:
            return self.ctx.erlang_atom("DFLAG_DIST_DEFAULT")


@dataclass
class TestConfigTraffic:
    ctx: Context = field(repr=False)
    schema: TestConfigTrafficSchema = field(repr=False)

    @property
    def key(self) -> str:
        return self.schema.key
