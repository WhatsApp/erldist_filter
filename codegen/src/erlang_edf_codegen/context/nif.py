# Copyright (c) Meta Platforms, Inc. and affiliates.
# Copyright (c) WhatsApp LLC
#
# This source code is licensed under the MIT license found in the
# LICENSE.md file in the root directory of this source tree.

"""Context NIF models for codegen."""
import bisect
from abc import abstractclassmethod
from collections import OrderedDict
from dataclasses import dataclass, field
from typing import Any, Iterator, Optional

from . import Atom, Context


@dataclass
class NIF:
    ctx: Context = field(repr=False)
    # _raw_config: Any = field(repr=False)
    atoms_dict: OrderedDict[str, Atom] = field(init=False)
    atoms_list: list[Atom] = field(init=False)
    # stats: "Stats" = field(init=False)

    def __post_init__(self) -> None:
        # from .stats import Stats

        self.atoms_dict = OrderedDict()
        self.atoms_list = []
        # self.stats = Stats(self, self._raw_config["stats"])

    def make_atom(self, name: str) -> Atom:
        if name not in self.atoms_dict:
            atom: Atom = Atom(self.ctx, name)
            self.atoms_dict[name] = atom
            bisect.insort(self.atoms_list, atom, key=lambda x: x.name)
        return self.atoms_dict[name]
