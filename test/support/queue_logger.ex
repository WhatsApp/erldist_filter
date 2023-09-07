# Copyright (c) Meta Platforms, Inc. and affiliates.
# Copyright (c) WhatsApp LLC
#
# This source code is licensed under the MIT license found in the
# LICENSE.md file in the root directory of this source tree.

defmodule ErldistFilterElixirTests.QueueLogger do
  @behaviour :erldist_filter_logger

  @spec child_spec() :: :supervisor.child_spec()
  def child_spec() do
    :erldist_filter_logger_sup.child_spec(__MODULE__, %{}, 1)
  end

  def export() do
    server_ref = :erldist_filter_logger.child_name(__MODULE__, 1)
    {:ok, reply} = :gen.call(server_ref, :"$erldist_filter_logger_call", :export)
    reply
  end

  @impl :erldist_filter_logger
  def init(_handler_options, _worker_number) do
    state = :queue.new()
    {:ok, state}
  end

  @impl :erldist_filter_logger
  def handle_batch(_batch_size, _batch_drop, batch_events, state) do
    {:handle_events, batch_events, state}
  end

  @impl :erldist_filter_logger
  def handle_control_event(time, sysname, control, state) do
    control = :udist.cast_to_dop(control)
    state = :queue.in({time, sysname, control}, state)
    {:cont, state}
  end

  @impl :erldist_filter_logger
  def handle_payload_event(time, sysname, control, payload, state) do
    control = :udist.cast_to_dop(control)
    state = :queue.in({time, sysname, control, payload}, state)
    {:cont, state}
  end

  @impl :erldist_filter_logger
  def handle_info({:"$erldist_filter_logger_call", from, :export}, state) do
    reply = :queue.to_list(state)
    :ok = :gen.reply(from, reply)
    state = :queue.new()
    {:cont, state}
  end

  def handle_info(_info, state) do
    {:cont, state}
  end

  @impl :erldist_filter_logger
  def terminate(_reason, _state) do
    :ok
  end
end
