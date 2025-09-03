# Copyright (c) Meta Platforms, Inc. and affiliates.
# Copyright (c) WhatsApp LLC
#
# This source code is licensed under the MIT license found in the
# LICENSE.md file in the root directory of this source tree.

"""Models for codegen."""
from enum import Enum
from pydantic import BaseModel, ConfigDict, Field, PositiveInt


class Model(BaseModel):
    model_config = ConfigDict(extra="forbid")


class Root(Model):
    channel: "Channel"
    config: "Config"
    dist: "Dist"
    dpi: "Dpi"
    test: "Test"


class Channel(Model):
    stats: list["ChannelStat"] = Field(default_factory=list)
    stats_dop: list["ChannelStatDop"] = Field(default_factory=list)


class ChannelStat(Model):
    key: str


class ChannelStatDop(Model):
    key: str


class Config(Model):
    fields: list["ConfigField"] = Field(default_factory=list)


class ConfigFieldKind(str, Enum):
    boolean = "boolean"


class ConfigField(Model):
    key: str
    kind: ConfigFieldKind


class Dist(Model):
    flags: list["DistFlag"] = Field(default_factory=list)
    extra_flags: list["DistExtraFlag"] = Field(default_factory=list)
    operations: list["DistOperation"] = Field(default_factory=list)
    spawn_flags: list["DistSpawnFlag"] = Field(default_factory=list)
    altact_sig_flags: list["DistAltactSigFlag"] = Field(default_factory=list)


class DistFlag(Model):
    key: str
    value: int
    internal: bool = False
    comment: str | None = None


class DistExtraFlag(Model):
    key: str
    values: list[str] = Field(default_factory=list)
    internal: bool = False
    comment: str | None = None


class DistOperation(Model):
    key: str
    value: int
    comment: str | None = None


class DistSpawnFlag(Model):
    key: str
    value: int
    comment: str | None = None


class DistAltactSigFlag(Model):
    key: str
    value: int
    comment: str | None = None


class Dpi(Model):
    otp_name_blocklist: list[str] = Field(default_factory=list)


class Test(Model):
    config: "TestConfig"


class TestConfig(Model):
    dpi: list["TestConfigDpi"] = Field(default_factory=list)
    headers: list["TestConfigHeader"] = Field(default_factory=list)
    traffic: list["TestConfigTraffic"] = Field(default_factory=list)


class TestConfigDpi(Model):
    key: str
    setup: str | None = None
    packets: list["TestConfigDpiPacket"] = Field(default_factory=list)


class TestConfigDpiPacket(Model):
    call: str
    drop: bool = Field(default=False)
    payload: str | None = None
    log: bool = Field(default=True)


class TestConfigHeaderMode(str, Enum):
    fragment = "fragment"
    normal = "normal"
    pass_through = "pass_through"


class TestConfigHeader(Model):
    key: str
    label: str
    mode: TestConfigHeaderMode
    fragment_size: PositiveInt | None = None


class TestConfigTraffic(Model):
    key: str
