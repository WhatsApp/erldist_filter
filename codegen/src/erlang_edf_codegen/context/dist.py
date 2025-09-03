# Copyright (c) Meta Platforms, Inc. and affiliates.
# Copyright (c) WhatsApp LLC
#
# This source code is licensed under the MIT license found in the
# LICENSE.md file in the root directory of this source tree.

"""Context Dist models for codegen."""
import bisect
from abc import abstractclassmethod
from collections import OrderedDict
from dataclasses import dataclass, field
from typing import Any, Iterator, Optional, Union

from ..schema import (
    Dist as DistSchema,
    DistFlag as DistFlagSchema,
    DistExtraFlag as DistExtraFlagSchema,
    DistOperation as DistOperationSchema,
    DistSpawnFlag as DistSpawnFlagSchema,
    DistAltactSigFlag as DistAltactSigFlagSchema,
)
from . import Atom, Context


DistAnyFlag = Union["DistFlag", "DistExtraFlag"]


@dataclass
class Dist:
    ctx: Context = field(repr=False)
    schema: DistSchema = field(repr=False)
    flags_dict: OrderedDict[str, DistAnyFlag] = field(init=False)
    flags_list: list[DistAnyFlag] = field(init=False)
    operations_dict: OrderedDict[str, "DistOperation"] = field(init=False)
    operations_list: list["DistOperation"] = field(init=False)
    spawn_flags_dict: OrderedDict[str, "DistSpawnFlag"] = field(init=False)
    spawn_flags_list: list["DistSpawnFlag"] = field(init=False)
    altact_sig_flags_dict: OrderedDict[str, "DistAltactSigFlag"] = field(init=False)
    altact_sig_flags_list: list["DistAltactSigFlag"] = field(init=False)

    def __post_init__(self) -> None:
        self.flags_dict = OrderedDict()
        self.flags_list = []

        for flag_schema in self.schema.flags:
            if flag_schema.key in self.flags_dict:
                raise ValueError(f"Duplicate DistFlag key: {flag_schema.key}")
            dist_flag: DistFlag = DistFlag(ctx=self.ctx, schema=flag_schema)
            self.flags_dict[dist_flag.key] = dist_flag
            self.flags_list.append(dist_flag)

        for extra_flag_schema in self.schema.extra_flags:
            if extra_flag_schema.key in self.flags_dict:
                raise ValueError(f"Duplicate DistExtraFlag key: {extra_flag_schema.key}")
            dist_extra_flag: DistExtraFlag = DistExtraFlag(ctx=self.ctx, parent=self, schema=extra_flag_schema)
            self.flags_dict[dist_extra_flag.key] = dist_extra_flag
            self.flags_list.append(dist_extra_flag)

        self.operations_dict = OrderedDict()
        self.operations_list = []

        for operation_schema in self.schema.operations:
            if operation_schema.key in self.operations_dict:
                raise ValueError(f"Duplicate DistOperation key: {operation_schema.key}")
            dist_operation: DistOperation = DistOperation(ctx=self.ctx, schema=operation_schema)
            self.operations_dict[dist_operation.key] = dist_operation
            self.operations_list.append(dist_operation)

        self.spawn_flags_dict = OrderedDict()
        self.spawn_flags_list = []

        for spawn_flag_schema in self.schema.spawn_flags:
            if spawn_flag_schema.key in self.spawn_flags_dict:
                raise ValueError(f"Duplicate DistSpawnFlag key: {spawn_flag_schema.key}")
            dist_spawn_flag: DistSpawnFlag = DistSpawnFlag(ctx=self.ctx, schema=spawn_flag_schema)
            self.spawn_flags_dict[dist_spawn_flag.key] = dist_spawn_flag
            self.spawn_flags_list.append(dist_spawn_flag)

        self.altact_sig_flags_dict = OrderedDict()
        self.altact_sig_flags_list = []

        for altact_sig_flag_schema in self.schema.altact_sig_flags:
            if altact_sig_flag_schema.key in self.altact_sig_flags_dict:
                raise ValueError(f"Duplicate DistSpawnFlag key: {altact_sig_flag_schema.key}")
            dist_altact_sig_flag: DistAltactSigFlag = DistAltactSigFlag(ctx=self.ctx, schema=altact_sig_flag_schema)
            self.altact_sig_flags_dict[dist_altact_sig_flag.key] = dist_altact_sig_flag
            self.altact_sig_flags_list.append(dist_altact_sig_flag)


@dataclass
class DistFlag:
    ctx: Context = field(repr=False)
    schema: DistFlagSchema = field(repr=False)

    @property
    def is_literal(self) -> bool:
        return True

    @property
    def key(self) -> str:
        return self.schema.key

    @property
    def value(self) -> int:
        return self.schema.value

    @property
    def internal(self) -> bool:
        return self.schema.internal

    @property
    def comment(self) -> str | None:
        return self.schema.comment


@dataclass
class DistExtraFlag:
    ctx: Context = field(repr=False)
    parent: Dist = field(repr=False)
    schema: DistExtraFlagSchema = field(repr=False)
    values_dict: OrderedDict[str, DistAnyFlag] = field(init=False, repr=False)
    values_list: list[DistAnyFlag] = field(init=False, repr=False)

    def __post_init__(self) -> None:
        self.values_dict = OrderedDict()
        self.values_list = []
        for value in self.schema.values:
            if value not in self.parent.flags_dict:
                raise ValueError(f"Unknown DistAnyFlag key: {value}")
            flag: DistAnyFlag = self.parent.flags_dict[value]
            if flag.key in self.values_dict:
                raise ValueError(f"Duplicate DistAnyFlag key on DistExtraFlag {self.key}: {flag.key}")
            self.values_dict[flag.key] = flag
            bisect.insort(self.values_list, flag, key=lambda x: x.key)

    @property
    def is_literal(self) -> bool:
        return False

    @property
    def key(self) -> str:
        return self.schema.key

    @property
    def internal(self) -> bool:
        return self.schema.internal

    @property
    def comment(self) -> str | None:
        return self.schema.comment


@dataclass
class DistOperation:
    ctx: Context = field(repr=False)
    schema: DistOperationSchema = field(repr=False)

    @property
    def key(self) -> str:
        return self.schema.key

    @property
    def value(self) -> int:
        return self.schema.value

    @property
    def comment(self) -> str | None:
        return self.schema.comment


@dataclass
class DistSpawnFlag:
    ctx: Context = field(repr=False)
    schema: DistSpawnFlagSchema = field(repr=False)

    @property
    def key(self) -> str:
        return self.schema.key

    @property
    def value(self) -> int:
        return self.schema.value

    @property
    def comment(self) -> str | None:
        return self.schema.comment


@dataclass
class DistAltactSigFlag:
    ctx: Context = field(repr=False)
    schema: DistAltactSigFlagSchema = field(repr=False)

    @property
    def key(self) -> str:
        return self.schema.key

    @property
    def value(self) -> int:
        return self.schema.value

    @property
    def comment(self) -> str | None:
        return self.schema.comment
