# Copyright (c) Meta Platforms, Inc. and affiliates.
# Copyright (c) WhatsApp LLC
#
# This source code is licensed under the MIT license found in the
# LICENSE.md file in the root directory of this source tree.

defmodule PeersTest do
  use ExUnit.Case, async: false

  @after_compile __MODULE__

  @peers ErldistFilterElixirTests.Peers

  require EDF

  test "alias_send dist operations function correctly" do
    {:ok, {upeer = {unode, _}, vpeer}} = @peers.setup(__MODULE__)

    assert([] === @peers.logger_export(upeer))
    assert([] === @peers.logger_export(vpeer))
    assert([] === @peers.handler_export(upeer))
    assert([] === @peers.handler_export(vpeer))

    small_token = :rand.bytes(16)
    large_token = :binary.copy(small_token, 40960)

    assert(
      :pong ===
        @peers.test(upeer, vpeer, fn vnode ->
          unode = node()
          ualias = :erlang.alias()
          uparent = self()
          req_id = :erlang.spawn_request(vnode, __MODULE__, :vnode_alias_send_process_init, [uparent, ualias], reply: :yes)

          {_monitor_ref, valias} =
            receive do
              {:spawn_reply, ^req_id, :ok, vpid} ->
                receive do
                  {^uparent, ^vpid, valias} ->
                    {req_id, valias}
                end

              {:spawn_reply, ^req_id, :error, reason} ->
                :erlang.error(reason)
            end

          _ = send(valias, {ualias, unode, {:ping, small_token}})
          _ = send(valias, {ualias, unode, {:ping, large_token}})

          :ok =
            receive do
              {^valias, ^vnode, {:pong, ^small_token}} ->
                :ok
            end

          :ok =
            receive do
              {^valias, ^vnode, {:pong, ^large_token}} ->
                :ok
            end

          _ = send(valias, {ualias, unode, :stop})

          :ok =
            receive do
              {^valias, ^vnode, :stopped} ->
                :ok
            end

          :pong
        end)
    )

    assert([] === @peers.logger_export(upeer))

    assert(
      match?(
        [
          {_, ^unode, EDF.udist_dop_spawn_request(mfa: {__MODULE__, :vnode_alias_send_process_init, 2}), [_uparent, _ualias]}
        ],
        @peers.logger_export(vpeer)
      )
    )

    assert([] === @peers.handler_export(upeer))

    assert(
      match?(
        [{:spawn_request_init, :keep, ^unode, __MODULE__, :vnode_alias_send_process_init, [_, _]}],
        @peers.handler_export(vpeer)
      )
    )

    :ok = @peers.teardown({upeer, vpeer})
  end

  def vnode_alias_send_process_init(uparent, ualias) do
    valias = :erlang.alias()
    _ = send(uparent, {uparent, self(), valias})
    vnode_alias_send_process_loop(valias, ualias)
  end

  defp vnode_alias_send_process_loop(valias, ualias) do
    receive do
      {^ualias, unode, {:ping, token}} when unode !== node() ->
        _ = send(ualias, {valias, node(), {:pong, token}})
        vnode_alias_send_process_loop(valias, ualias)

      {^ualias, unode, :stop} when unode !== node() ->
        true = :erlang.unalias(valias)
        _ = send(ualias, {valias, node(), :stopped})
        exit(:normal)
    end
  end

  test "reg_send dist operations function correctly" do
    {:ok, {upeer = {unode, _}, vpeer}} = @peers.setup(__MODULE__)

    assert([] === @peers.logger_export(upeer))
    assert([] === @peers.logger_export(vpeer))
    assert([] === @peers.handler_export(upeer))
    assert([] === @peers.handler_export(vpeer))

    reg_name = @peers.rand_reg_name(upeer, vpeer)
    small_token = :rand.bytes(16)
    large_token = :binary.copy(small_token, 40960)

    assert(
      :pong ===
        @peers.test(upeer, vpeer, fn vnode ->
          unode = node()
          uparent = self()
          true = :erlang.register(reg_name, self())
          req_id = :erlang.spawn_request(vnode, __MODULE__, :vnode_registered_process_init, [uparent, reg_name], reply: :yes)

          _monitor_ref =
            receive do
              {:spawn_reply, ^req_id, :ok, vpid} ->
                receive do
                  {^uparent, ^vpid, ^reg_name} ->
                    req_id
                end

              {:spawn_reply, ^req_id, :error, reason} ->
                :erlang.error(reason)
            end

          _ = send({reg_name, vnode}, {reg_name, unode, {:ping, small_token}})
          _ = send({reg_name, vnode}, {reg_name, unode, {:ping, large_token}})

          :ok =
            receive do
              {^reg_name, ^vnode, {:pong, ^small_token}} ->
                :ok
            end

          :ok =
            receive do
              {^reg_name, ^vnode, {:pong, ^large_token}} ->
                :ok
            end

          _ = send({reg_name, vnode}, {reg_name, unode, :stop})

          :ok =
            receive do
              {^reg_name, ^vnode, :stopped} ->
                :ok
            end

          true = :erlang.unregister(reg_name)

          :pong
        end)
    )

    assert([] === @peers.logger_export(upeer))

    assert(
      match?(
        [
          {_, ^unode, EDF.udist_dop_spawn_request(mfa: {__MODULE__, :vnode_registered_process_init, 2}), [_uparent, ^reg_name]}
        ],
        @peers.logger_export(vpeer)
      )
    )

    assert([] === @peers.handler_export(upeer))

    assert(
      match?(
        [{:spawn_request_init, :keep, ^unode, __MODULE__, :vnode_registered_process_init, [_, _]}],
        @peers.handler_export(vpeer)
      )
    )

    :ok = @peers.teardown({upeer, vpeer})
  end

  def vnode_registered_process_init(uparent, reg_name) do
    true = :erlang.register(reg_name, self())
    _ = send(uparent, {uparent, self(), reg_name})
    vnode_registered_process_loop(reg_name)
  end

  defp vnode_registered_process_loop(reg_name) do
    receive do
      {^reg_name, unode, {:ping, token}} when unode !== node() ->
        _ = send({reg_name, unode}, {reg_name, node(), {:pong, token}})
        vnode_registered_process_loop(reg_name)

      {^reg_name, unode, :stop} when unode !== node() ->
        true = :erlang.unregister(reg_name)
        _ = send({reg_name, unode}, {reg_name, node(), :stopped})
        exit(:normal)
    end
  end

  test "send_sender dist operations function correctly" do
    {:ok, {upeer = {unode, _}, vpeer}} = @peers.setup(__MODULE__)

    assert([] === @peers.logger_export(upeer))
    assert([] === @peers.logger_export(vpeer))
    assert([] === @peers.handler_export(upeer))
    assert([] === @peers.handler_export(vpeer))

    small_token = :rand.bytes(16)
    large_token = :binary.copy(small_token, 40960)

    assert(
      :pong ===
        @peers.test(upeer, vpeer, fn vnode ->
          unode = node()
          upid = self()
          uparent = self()
          req_id = :erlang.spawn_request(vnode, __MODULE__, :vnode_send_sender_process_init, [uparent, upid], reply: :yes)

          {_monitor_ref, vpid} =
            receive do
              {:spawn_reply, ^req_id, :ok, vpid} ->
                receive do
                  {^uparent, ^vpid, ^vpid} ->
                    {req_id, vpid}
                end

              {:spawn_reply, ^req_id, :error, reason} ->
                :erlang.error(reason)
            end

          _ = send(vpid, {upid, unode, {:ping, small_token}})
          _ = send(vpid, {upid, unode, {:ping, large_token}})

          :ok =
            receive do
              {^vpid, ^vnode, {:pong, ^small_token}} ->
                :ok
            end

          :ok =
            receive do
              {^vpid, ^vnode, {:pong, ^large_token}} ->
                :ok
            end

          _ = send(vpid, {upid, unode, :stop})

          :ok =
            receive do
              {^vpid, ^vnode, :stopped} ->
                :ok
            end

          :pong
        end)
    )

    assert([] === @peers.logger_export(upeer))

    assert(
      match?(
        [
          {_, ^unode, EDF.udist_dop_spawn_request(mfa: {__MODULE__, :vnode_send_sender_process_init, 2}), [_uparent, _upid]}
        ],
        @peers.logger_export(vpeer)
      )
    )

    assert([] === @peers.handler_export(upeer))

    assert(
      match?(
        [{:spawn_request_init, :keep, ^unode, __MODULE__, :vnode_send_sender_process_init, [_, _]}],
        @peers.handler_export(vpeer)
      )
    )

    :ok = @peers.teardown({upeer, vpeer})
  end

  def vnode_send_sender_process_init(uparent, upid) do
    vpid = self()
    _ = send(uparent, {uparent, self(), vpid})
    vnode_send_sender_process_loop(vpid, upid)
  end

  defp vnode_send_sender_process_loop(vpid, upid) do
    receive do
      {^upid, unode, {:ping, token}} when unode !== node() ->
        _ = send(upid, {vpid, node(), {:pong, token}})
        vnode_send_sender_process_loop(vpid, upid)

      {^upid, unode, :stop} when unode !== node() ->
        _ = send(upid, {vpid, node(), :stopped})
        exit(:normal)
    end
  end

  def __after_compile__(env, bytecode) do
    _ = :persistent_term.put(__MODULE__, {__MODULE__, bytecode, :unicode.characters_to_list(env.file)})
    :ok
  end
end
