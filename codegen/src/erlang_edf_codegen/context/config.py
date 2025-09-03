# Copyright (c) Meta Platforms, Inc. and affiliates.
# Copyright (c) WhatsApp LLC
#
# This source code is licensed under the MIT license found in the
# LICENSE.md file in the root directory of this source tree.

"""Context Config models for codegen."""
import bisect
from abc import abstractclassmethod
from collections import OrderedDict
from dataclasses import dataclass, field
from typing import Any, Iterator, Optional, Union

from ..schema import (
    Config as ConfigSchema,
    ConfigField as ConfigFieldSchema,
)
from . import Atom, Context


@dataclass
class Config:
    ctx: Context = field(repr=False)
    schema: ConfigSchema = field(repr=False)
    fields_dict: OrderedDict[str, "ConfigField"] = field(init=False)
    fields_list: list["ConfigField"] = field(init=False)

    def __post_init__(self) -> None:
        self.fields_dict = OrderedDict()
        self.fields_list = []

        for field_schema in self.schema.fields:
            if field_schema.key in self.fields_dict:
                raise ValueError(f"Duplicate ConfigField key: {field_schema.key}")
            field = ConfigField(ctx=self.ctx, schema=field_schema)
            self.fields_dict[field.key] = field
            self.fields_list.append(field)


@dataclass
class ConfigField:
    ctx: Context = field(repr=False)
    schema: ConfigFieldSchema = field(repr=False)

    @property
    def key(self) -> str:
        return self.schema.key

    @property
    def kind(self) -> str:
        return self.schema.kind.value
