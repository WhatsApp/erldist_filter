# Copyright (c) Meta Platforms, Inc. and affiliates.
# Copyright (c) WhatsApp LLC
#
# This source code is licensed under the MIT license found in the
# LICENSE.md file in the root directory of this source tree.

import? 'justfiles/docker.just'
import? 'justfiles/python.just'
import? 'justfiles/tools.just'

default:
    just --list
