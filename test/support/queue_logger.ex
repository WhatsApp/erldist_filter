# Copyright (c) Meta Platforms, Inc. and affiliates.
# Copyright (c) WhatsApp LLC
#
# This source code is licensed under the MIT license found in the
# LICENSE.md file in the root directory of this source tree.

defmodule ErldistFilterElixirTests.QueueLogger do
  @call_timeout :timer.seconds(15)

  @behaviour :erldist_filter_logger

  @spec child_spec() :: :supervisor.child_spec()
  def child_spec() do
    :erldist_filter_logger_sup.child_spec(__MODULE__, %{}, 1)
  end

  def dequeue() do
    server_ref = :erldist_filter_logger.child_name(__MODULE__, 1)
    {:ok, reply} = :gen.call(server_ref, :"$erldist_filter_logger_call", :dequeue, @call_timeout)
    reply
  end

  def dequeue(0), do: []
  def dequeue(n) when is_integer(n) and n > 0, do: [dequeue() | dequeue(n - 1)]

  def export() do
    server_ref = :erldist_filter_logger.child_name(__MODULE__, 1)
    {:ok, reply} = :gen.call(server_ref, :"$erldist_filter_logger_call", :export, @call_timeout)
    reply
  end

  defmodule State do
    defstruct dequeue: :queue.new(), enqueue: :queue.new()

    @type t() :: %__MODULE__{
            dequeue: :queue.queue(:gen_statem.from()),
            enqueue: :queue.queue(dynamic())
          }
  end

  @impl :erldist_filter_logger
  def init(_handler_options, _worker_number) do
    state = %State{}
    {:ok, state}
  end

  @impl :erldist_filter_logger
  def handle_batch(_batch_size, _batch_drop, batch_events, state) do
    {:handle_events, batch_events, state}
  end

  @impl :erldist_filter_logger
  def handle_control_event(time, sysname, control, state) do
    control = :udist.cast_to_dop(control)
    state = enqueue({time, sysname, control}, state)
    {:cont, state}
  end

  @impl :erldist_filter_logger
  def handle_payload_event(time, sysname, control, payload, state) do
    control = :udist.cast_to_dop(control)
    state = enqueue({time, sysname, control, payload}, state)
    {:cont, state}
  end

  @impl :erldist_filter_logger
  def handle_info({:"$erldist_filter_logger_call", from, :dequeue}, state = %State{dequeue: dequeue, enqueue: enqueue}) do
    case :queue.out(enqueue) do
      {:empty, ^enqueue} ->
        dequeue = :queue.in(from, dequeue)
        state = %{state | dequeue: dequeue}
        {:cont, state}

      {{:value, event}, enqueue} ->
        :ok = :gen.reply(from, event)
        state = %{state | enqueue: enqueue}
        {:cont, state}
    end
  end

  def handle_info({:"$erldist_filter_logger_call", from, :export}, state = %State{enqueue: enqueue}) do
    reply = :queue.to_list(enqueue)
    :ok = :gen.reply(from, reply)
    enqueue = :queue.new()
    state = %{state | enqueue: enqueue}
    {:cont, state}
  end

  def handle_info(_info, state) do
    {:cont, state}
  end

  @impl :erldist_filter_logger
  def terminate(_reason, _state) do
    :ok
  end

  @doc false
  defp enqueue(message, state = %State{dequeue: dequeue, enqueue: enqueue}) do
    case :queue.out(dequeue) do
      {:empty, ^dequeue} ->
        enqueue = :queue.in(message, enqueue)
        state = %{state | enqueue: enqueue}
        state

      {{:value, from}, dequeue} ->
        :ok = :gen.reply(from, message)
        state = %{state | dequeue: dequeue}
        state
    end
  end
end
