# Copyright (c) Meta Platforms, Inc. and affiliates.
# Copyright (c) WhatsApp LLC
#
# This source code is licensed under the MIT license found in the
# LICENSE.md file in the root directory of this source tree.

# :ok = :logger.set_primary_config(:level, :debug)
# :ok = :lists.foreach(&:logger.remove_handler/1, :logger.get_handler_ids)
# :ok = :logger.add_handler(:debug_output, :logger_std_h, %{
#   config: %{type: :standard_error},
#   formatter: {:logger_formatter, %{legacy_header: false, single_line: true}},
#   level: :debug
# })

ExUnit.start(max_cases: 1)
