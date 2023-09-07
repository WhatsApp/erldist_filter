# Copyright (c) Meta Platforms, Inc. and affiliates.
# Copyright (c) WhatsApp LLC
#
# This source code is licensed under the MIT license found in the
# LICENSE.md file in the root directory of this source tree.

defmodule ErldistFilterElixirTests.QueueHandler do
  @behaviour :erldist_filter_handler
  @behaviour :gen_statem

  @spec child_spec() :: :supervisor.child_spec()
  def child_spec() do
    %{
      id: __MODULE__,
      start: {__MODULE__, :start_link, []},
      restart: :permanent,
      shutdown: 5000,
      type: :worker,
      modules: [__MODULE__]
    }
  end

  @spec start_link() :: :gen_statem.start_ret()
  def start_link() do
    :gen_statem.start_link({:local, __MODULE__}, __MODULE__, {}, [])
  end

  @impl :gen_statem
  def callback_mode() do
    [:handle_event_function]
  end

  @impl :gen_statem
  def init({}) do
    data = :queue.new()
    {:ok, nil, data}
  end

  @impl :gen_statem
  def handle_event({:call, from}, :export, _state, data) do
    reply = :queue.to_list(data)
    data = :queue.new()
    actions = [{:reply, from, reply}]
    {:keep_state, data, actions}
  end

  def handle_event(:cast, {:enqueue, message}, _state, data) do
    data = :queue.in(message, data)
    {:keep_state, data}
  end

  def enqueue(message) do
    :gen_statem.cast(__MODULE__, {:enqueue, message})
  end

  def export() do
    :gen_statem.call(__MODULE__, :export)
  end

  @impl :erldist_filter_handler
  def classify(hint, sysname, control) do
    action =
      case hint do
        :drop -> :drop
        :safe -> :keep
        :unsafe -> :drop
      end

    :ok = enqueue({:classify, hint, action, sysname, control})
    action
  end

  @impl :erldist_filter_handler
  def classify(hint, sysname, control, payload) do
    action =
      case hint do
        :drop ->
          # Return unauthorized for $gen_call to speed up testing.
          case payload do
            {:"$gen_call", from, _request} ->
              :ok =
                try do
                  _ = :gen.reply(from, :unauthorized)
                  :ok
                catch
                  _, _ ->
                    :ok
                end

              :drop

            _ ->
              :drop
          end

        :safe ->
          :keep

        :unsafe ->
          case :erldist_filter_nif.config_get(:untrusted) do
            false ->
              :keep

            true ->
              # Block I/O requests when in untrusted mode.
              case payload do
                {:io_reply, _reply_as, _reply} ->
                  :keep

                {:io_request, from, reply_as, _request} ->
                  :ok =
                    try do
                      case node(from) do
                        ^sysname ->
                          _ = Process.send(from, {:io_reply, reply_as, {:error, :enotsup}}, [:noconnect])
                          :ok

                        _ ->
                          :ok
                      end
                    catch
                      _, _ ->
                        :ok
                    end

                  :drop

                _ ->
                  :keep
              end
          end
      end

    :ok = enqueue({:classify, hint, action, sysname, control, payload})
    action
  end

  @impl :erldist_filter_handler
  def spawn_request_init(sysname, module, function_name, arguments) do
    case :erldist_filter_nif.config_get(:untrusted) do
      false ->
        :ok = enqueue({:spawn_request_init, :keep, sysname, module, function_name, arguments})
        :erlang.apply(module, function_name, arguments)

      true ->
        :ok = enqueue({:spawn_request_init, :drop, sysname, module, function_name, arguments})

        case {module, function_name, arguments} do
          {:erpc, :execute_call, [ref, _module, _function, _arguments]} ->
            :erpc.execute_call(ref, __MODULE__, :spawn_request_unauthorized, [])

          _ ->
            exit(:normal)
        end
    end
  end

  def spawn_request_unauthorized() do
    :unauthorized
  end
end
