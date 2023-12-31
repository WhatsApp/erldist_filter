# Copyright (c) Meta Platforms, Inc. and affiliates.
# Copyright (c) WhatsApp LLC
#
# This source code is licensed under the MIT license found in the
# LICENSE.md file in the root directory of this source tree.

defmodule ErldistFilterElixirTests.Peers do
  @sup :erldist_filter_peer_spbt_sup
  @shim :erldist_filter_peer_spbt_shim
  @handler ErldistFilterElixirTests.QueueHandler
  @logger ErldistFilterElixirTests.QueueLogger

  def setup(test_module) do
    {:ok, {^test_module, test_bytecode, test_filename}} = read_module(test_module)
    {:ok, _} = force_clean_start_supervisor()
    # {:ok, _} = :supervisor.start_child(:kernel_sup, @sup.child_spec())
    {:ok, {upeer, vpeer}} = @shim.start_random_suffix_upeer_and_vpeer_from_label(~c"edf-elixir")
    :ok = rpc(upeer, __MODULE__, :enable_erldist_filter, [])
    :ok = rpc(vpeer, __MODULE__, :enable_erldist_filter, [])
    {:module, ^test_module} = rpc(upeer, :code, :load_binary, [test_module, test_filename, test_bytecode])
    {:module, ^test_module} = rpc(vpeer, :code, :load_binary, [test_module, test_filename, test_bytecode])
    {:ok, {upeer, vpeer}}
  end

  defp force_clean_start_supervisor() do
    case :supervisor.start_child(:kernel_sup, @sup.child_spec()) do
      {:ok, sup_pid} ->
        {:ok, sup_pid}

      {:error, {:already_started, sup_pid}} ->
        force_clean_start_supervisor(sup_pid, :supervisor.which_children(sup_pid))

      error ->
        error
    end
  end

  defp force_clean_start_supervisor(sup_pid, [{id, _child, _type, _modules} | children]) do
    :ok = @sup.stop_child(id)
    force_clean_start_supervisor(sup_pid, children)
  end

  defp force_clean_start_supervisor(sup_pid, []) do
    {:ok, sup_pid}
  end

  defp read_module(module) do
    case :code.get_object_code(module) do
      {^module, bytecode, filename} ->
        {:ok, {module, bytecode, filename}}

      :error ->
        case :persistent_term.get(module) do
          {^module, bytecode, filename} when is_binary(bytecode) ->
            {:ok, {module, bytecode, filename}}
        end
    end
  end

  def teardown({upeer = {upeer_node, _upeer_pid}, vpeer = {vpeer_node, _vpeer_pid}}) do
    :ok = rpc(upeer, __MODULE__, :disable_erldist_filter, [])
    :ok = rpc(vpeer, __MODULE__, :disable_erldist_filter, [])
    :ok = @sup.stop_child(vpeer_node)
    :ok = @sup.stop_child(upeer_node)
    # _ = :supervisor.terminate_child(:kernel_sup, @sup)
    # _ = :supervisor.delete_child(:kernel_sup, @sup)
    :ok
  end

  def rpc(peer, function) when is_function(function, 0) do
    rpc(peer, :erlang, :apply, [function, []])
  end

  def rpc(peer, function, timeout) when is_function(function, 0) do
    rpc(peer, :erlang, :apply, [function, []], timeout)
  end

  def rpc(peer, module, function, arguments) do
    @shim.rpc(peer, module, function, arguments)
  end

  def rpc(peer, module, function, arguments, timeout) do
    @shim.rpc(peer, module, function, arguments, timeout)
  end

  def handler_export(peer) do
    rpc(peer, @handler, :export, [])
  end

  def logger_export(peer) do
    rpc(peer, @logger, :export, [])
  end

  def peer_spawn_and_exec(peer_node, function) when is_function(function, 1) do
    parent = self()

    {pid, mon} =
      :erlang.spawn_opt(
        fn ->
          mon =
            receive do
              {^parent, startmon} ->
                startmon
            end

          reply = function.(peer_node)
          _ = send(mon, {mon, self(), reply})
          exit(:normal)
        end,
        monitor: [alias: :reply_demonitor]
      )

    _ = send(pid, {parent, mon})

    receive do
      {^mon, ^pid, reply} ->
        reply

      {:DOWN, ^mon, :process, ^pid, reason} ->
        exit(reason)
    end
  end

  def test(upeer, _vpeer = {vpeer_node, _vpeer_pid}, function) when is_function(function, 1) do
    rpc(upeer, __MODULE__, :peer_spawn_and_exec, [vpeer_node, function])
  end

  def enable_erldist_filter() do
    {:ok, _} = :application.ensure_all_started(:erldist_filter)
    _ = :erldist_filter.handler_set(@handler)
    _ = :erldist_filter_nif.logger_set_capacity(1000)
    :ok = :erldist_filter_nif.config_set(:compact_fragments, true)
    :ok = :erldist_filter_nif.config_set(:deep_packet_inspection, true)
    :ok = :erldist_filter_nif.config_set(:logging, true)
    :ok = :erldist_filter_nif.config_set(:redirect_dist_operations, true)
    {:ok, _} = :supervisor.start_child(:kernel_sup, @logger.child_spec())
    {:ok, _} = :supervisor.start_child(:kernel_sup, @handler.child_spec())
    :ok
  end

  def disable_erldist_filter() do
    _ = :supervisor.terminate_child(:kernel_sup, @handler)
    _ = :supervisor.delete_child(:kernel_sup, @handler)
    _ = :supervisor.terminate_child(:kernel_sup, @logger)
    _ = :supervisor.delete_child(:kernel_sup, @logger)
    _ = :erldist_filter_nif.logger_set_capacity(2)
    :ok = :erldist_filter_nif.config_set(:compact_fragments, false)
    :ok = :erldist_filter_nif.config_set(:deep_packet_inspection, false)
    :ok = :erldist_filter_nif.config_set(:logging, false)
    :ok = :erldist_filter_nif.config_set(:redirect_dist_operations, false)
    :ok = :erldist_filter_nif.config_set(:untrusted, false)
    # :ok = :application.stop(:erldist_filter)
    :ok
  end

  def enable_untrusted(peer) do
    rpc(peer, :erldist_filter_nif, :config_set, [:untrusted, true])
  end

  def disable_untrusted(peer) do
    rpc(peer, :erldist_filter_nif, :config_set, [:untrusted, false])
  end

  def rand_reg_name(upeer, vpeer) do
    reg_name = :erlang.binary_to_atom(utf8(rand_uniform(1, 255)), :utf8)

    if reg_name !== :"" and reg_name !== nil and reg_name !== :undefined and :erlang.whereis(reg_name) === :undefined and
         rpc(upeer, :erlang, :whereis, [reg_name]) === :undefined and rpc(vpeer, :erlang, :whereis, [reg_name]) === :undefined do
      reg_name
    else
      rand_reg_name(upeer, vpeer)
    end
  end

  def rand_uniform(lo, hi), do: :rand.uniform(hi - lo + 1) + lo - 1

  def unicode_codepoint(1), do: rand_uniform(0, 0x7F)
  def unicode_codepoint(2), do: rand_uniform(0x80, 0x7FF)

  def unicode_codepoint(3) do
    if :rand.uniform(2) === 1 do
      rand_uniform(0x800, 0xD7FF)
    else
      rand_uniform(0xE000, 0xFFFD)
    end
  end

  def unicode_codepoint(4), do: rand_uniform(0x10000, 0x10FFFF)

  def utf8(n) when is_integer(n) and n >= 0, do: :unicode.characters_to_binary(utf8_internal(n))

  defp utf8_internal(0), do: []
  defp utf8_internal(n) when is_integer(n) and n > 0, do: [unicode_codepoint(rand_uniform(1, 4)) | utf8_internal(n - 1)]
end
