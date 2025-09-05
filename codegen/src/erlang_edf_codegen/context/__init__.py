# Copyright (c) Meta Platforms, Inc. and affiliates.
# Copyright (c) WhatsApp LLC
#
# This source code is licensed under the MIT license found in the
# LICENSE.md file in the root directory of this source tree.

"""Context models for codegen."""
import abc
import bisect
from collections import OrderedDict
from dataclasses import dataclass, field
from datetime import datetime

# from pydantic import BaseModel, Field, PrivateAttr
from typing import Any, Iterator, Optional

from ..schema import Root as RootSchema


class NamesMixin:
    """A mixin that provides various name format variations."""

    # def __str__(self) -> str:
    #     return self.name

    @abc.abstractmethod
    def name_prefix(self) -> str:
        pass

    @abc.abstractmethod
    def names(self) -> list[str]:
        pass

    @property
    def dot_name(self) -> str:
        """Returns the name in dot.notation format."""
        return ".".join(self.names())

    @property
    def var_name(self) -> str:
        """Returns the name in snake_case format."""
        return "_".join(self.names())

    # @property
    # def full_underscore_name(self) -> str:
    #     return f"{self.name_prefix().lower()}_{self.underscore_name.lower()}"

    # @property
    # def macro_name(self) -> str:
    #     return f"{self.name_prefix().upper()}_{self.underscore_name.upper()}"

    # @property
    # def underscore_name(self) -> str:
    #     return "_".join(self.names())


@dataclass
class Atom:
    ctx: "Context" = field(repr=False)
    name: str


@dataclass
class Context:
    _output_path: str = field(repr=False)
    schema: RootSchema = field(repr=False)
    channel: "Channel" = field(init=False)
    config: "Config" = field(init=False)
    dist: "Dist" = field(init=False)
    dpi: "Dpi" = field(init=False)
    nif: "NIF" = field(init=False)
    test: "Test" = field(init=False)
    version: int = field(init=False)

    def __post_init__(self) -> None:
        from .channel import Channel
        from .config import Config
        from .dist import Dist
        from .dpi import Dpi
        from .nif import NIF
        from .test import Test

        self.version = int(datetime.now().strftime("%Y%m%d%H%M%S"))
        self.channel = Channel(ctx=self, schema=self.schema.channel)
        self.config = Config(ctx=self, schema=self.schema.config)
        self.dist = Dist(ctx=self, schema=self.schema.dist)
        self.dpi = Dpi(ctx=self, schema=self.schema.dpi)
        self.nif = NIF(ctx=self)
        self.test = Test(ctx=self, schema=self.schema.test)

    def debug(self, name: str) -> bool:
        if name in self.debug_names():
            return True
        else:
            return False

    def debug_names(self) -> set[str]:
        return set()

    def debug_print_fmt(self, str_fmt: Optional[str]) -> str:
        if str_fmt:
            return self.escape_string_for_c(f"%s:%d {str_fmt}")
        else:
            return self.escape_string_for_c("%s:%d")

    def debug_print_args(self, str_args: Optional[str]) -> str:
        if str_args:
            return f", {str_args}"
        else:
            return ""

    def erlang_atom(self, original_str: str) -> str:
        return f"'{original_str}'"

    def erlang_comment(self, original_str: str) -> str:
        comment: str = original_str.strip()
        lines: list[str] = comment.split("\n")
        return "\n".join(f"%% {line}" for line in lines)

    def erlang_hex(self, original_int: int) -> str:
        return f"16#{original_int:02X}"

    def escape_string_for_c(self, original_str: str) -> str:
        escaped_str: str = original_str.encode("unicode_escape").decode("utf-8")
        escaped_str = escaped_str.replace('"', r"\"")  # escape double quotes
        escaped_str = escaped_str.replace("'", r"\'")  # escape single quotes
        return f'"{escaped_str}"'
