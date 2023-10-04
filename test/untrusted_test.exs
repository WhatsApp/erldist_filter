# Copyright (c) Meta Platforms, Inc. and affiliates.
# Copyright (c) WhatsApp LLC
#
# This source code is licensed under the MIT license found in the
# LICENSE.md file in the root directory of this source tree.

defmodule UntrustedTest do
  use ExUnit.Case, async: false

  @after_compile __MODULE__

  @peers ErldistFilterElixirTests.Peers

  require EDF

  test "enabling untrusted mode captures extra traffic" do
    {:ok, {upeer = {unode, _}, vpeer = {vnode, _}}} = @peers.setup(__MODULE__)

    assert([] === @peers.logger_export(upeer))
    assert([] === @peers.logger_export(vpeer))
    assert([] === @peers.handler_export(upeer))
    assert([] === @peers.handler_export(vpeer))

    assert(match?(:pong, @peers.rpc(upeer, Node, :ping, [vnode])))
    assert(match?(:pong, @peers.rpc(vpeer, Node, :ping, [unode])))

    assert([] === @peers.logger_export(upeer))
    assert([] === @peers.logger_export(vpeer))
    assert([] === @peers.handler_export(upeer))
    assert([] === @peers.handler_export(vpeer))

    @peers.enable_untrusted(upeer)
    @peers.enable_untrusted(vpeer)

    assert(match?(:pong, @peers.rpc(upeer, Node, :ping, [vnode])))

    assert(
      match?(
        [
          {_, ^unode, EDF.udist_dop_monitor_p(to_proc: :net_kernel)},
          {_, ^unode, EDF.udist_dop_reg_send(to_name: :net_kernel), {:"$gen_call", _, {:is_auth, ^unode}}},
          {_, ^unode, EDF.udist_dop_demonitor_p(to_proc: :net_kernel)}
        ],
        @peers.logger_export(vpeer)
      )
    )

    assert(
      match?(
        [
          {:classify, :safe, :keep, ^unode, EDF.udist_dop_reg_send(to_name: :net_kernel), {:"$gen_call", _, {:is_auth, ^unode}}}
        ],
        @peers.handler_export(vpeer)
      )
    )

    assert(
      match?(
        [
          # OTP 26+ only
          # {_, ^vnode, EDF.udist_dop_alias_send(), {_, :yes}}
          {_, ^vnode, _, {_, :yes}}
        ],
        @peers.logger_export(upeer)
      )
    )

    assert(
      match?(
        [
          # OTP 26+ only
          # {:classify, :unsafe, :keep, ^vnode, EDF.udist_dop_alias_send(), {_, :yes}}
          {:classify, :unsafe, :keep, ^vnode, _, {_, :yes}}
        ],
        @peers.handler_export(upeer)
      )
    )

    @peers.disable_untrusted(upeer)
    @peers.disable_untrusted(vpeer)

    assert(match?(:pong, @peers.rpc(upeer, Node, :ping, [vnode])))
    assert(match?(:pong, @peers.rpc(vpeer, Node, :ping, [unode])))

    assert([] === @peers.logger_export(upeer))
    assert([] === @peers.logger_export(vpeer))
    assert([] === @peers.handler_export(upeer))
    assert([] === @peers.handler_export(vpeer))

    :ok = @peers.teardown({upeer, vpeer})
  end

  test "untrusted nodes cannot halt peers" do
    {:ok, {upeer = {unode, _}, vpeer = {vnode, _}}} = setup_untrusted()

    assert(match?(:unauthorized, @peers.rpc(upeer, :erpc, :call, [vnode, System, :halt, [1]])))

    assert(
      match?(
        [
          {_, ^unode, EDF.udist_dop_spawn_request(mfa: {:erpc, :execute_call, 4}), [_, System, :halt, [1]]}
        ],
        @peers.logger_export(vpeer)
      )
    )

    assert(
      match?(
        [
          {:spawn_request_init, :drop, ^unode, :erpc, :execute_call, [_, System, :halt, [1]]}
        ],
        @peers.handler_export(vpeer)
      )
    )

    assert(
      match?(
        [
          {_, ^vnode, EDF.udist_dop_spawn_reply(result: result)},
          {_, ^vnode, EDF.udist_dop_payload_monitor_p_exit(from_proc: result), {_, :return, :unauthorized}}
        ]
        when is_pid(result),
        @peers.logger_export(upeer)
      )
    )

    assert(match?([], @peers.handler_export(upeer)))

    teardown_untrusted({upeer, vpeer})
  end

  test "untrusted nodes cannot abuse supervisors on peers" do
    {:ok, {upeer = {unode, _}, vpeer = {vnode, _}}} = setup_untrusted()

    child_spec = %{
      id: :halter,
      start: {System, :halt, [1]}
    }

    assert(match?(:unauthorized, @peers.rpc(upeer, Supervisor, :start_child, [{:kernel_sup, vnode}, child_spec])))

    assert(
      match?(
        [
          {_, ^unode, EDF.udist_dop_monitor_p(to_proc: :kernel_sup)},
          {_, ^unode, EDF.udist_dop_reg_send(to_name: :kernel_sup), {:"$gen_call", _, {:start_child, ^child_spec}}},
          {_, ^unode, EDF.udist_dop_demonitor_p(to_proc: :kernel_sup)}
        ],
        @peers.logger_export(vpeer)
      )
    )

    assert(
      match?(
        [
          {:classify, :drop, :drop, ^unode, EDF.udist_dop_reg_send(to_name: :kernel_sup),
           {:"$gen_call", _, {:start_child, ^child_spec}}}
        ],
        @peers.handler_export(vpeer)
      )
    )

    assert(
      match?(
        [
          {_, ^vnode, EDF.udist_dop_alias_send(), {_, :unauthorized}}
        ],
        @peers.logger_export(upeer)
      )
    )

    assert(
      match?(
        [
          {:classify, :unsafe, :keep, ^vnode, EDF.udist_dop_alias_send(), {_, :unauthorized}}
        ],
        @peers.handler_export(upeer)
      )
    )

    teardown_untrusted({upeer, vpeer})
  end

  test "untrusted nodes cannot abuse I/O requests on peers" do
    {:ok, {upeer = {unode, _}, vpeer = {vnode, _}}} = setup_untrusted()

    assert(
      match?(
        {:io_reply, _, {:error, :enotsup}},
        @peers.rpc(upeer, fn ->
          request = {:put_chars, :unicode, System, :halt, [1]}
          io_request = {:io_request, self(), make_ref(), request}
          send({:standard_error, vnode}, io_request)

          receive do
            msg ->
              msg
          end
        end)
      )
    )

    assert(
      match?(
        [
          {_, ^unode, EDF.udist_dop_reg_send(to_name: :standard_error), {:io_request, _, _, _}}
        ],
        @peers.logger_export(vpeer)
      )
    )

    assert(
      match?(
        [
          {:classify, :drop, :drop, ^unode, EDF.udist_dop_reg_send(to_name: :standard_error), {:io_request, _, _, _}}
        ],
        @peers.handler_export(vpeer)
      )
    )

    assert(
      match?(
        [
          {_, ^vnode, EDF.udist_dop_send_sender(), {:io_reply, _, {:error, :enotsup}}}
        ],
        @peers.logger_export(upeer)
      )
    )

    assert(
      match?(
        [
          {:classify, :drop, :keep, ^vnode, EDF.udist_dop_send_sender(), {:io_reply, _, {:error, :enotsup}}}
        ],
        @peers.handler_export(upeer)
      )
    )

    teardown_untrusted({upeer, vpeer})
  end

  def setup_untrusted() do
    {:ok, {upeer = {unode, _}, vpeer = {vnode, _}}} = @peers.setup(__MODULE__)

    assert([] === @peers.logger_export(upeer))
    assert([] === @peers.logger_export(vpeer))
    assert([] === @peers.handler_export(upeer))
    assert([] === @peers.handler_export(vpeer))

    assert(match?(:pong, @peers.rpc(upeer, Node, :ping, [vnode])))
    assert(match?(:pong, @peers.rpc(vpeer, Node, :ping, [unode])))

    assert([] === @peers.logger_export(upeer))
    assert([] === @peers.logger_export(vpeer))
    assert([] === @peers.handler_export(upeer))
    assert([] === @peers.handler_export(vpeer))

    @peers.enable_untrusted(upeer)
    @peers.enable_untrusted(vpeer)

    {:ok, {upeer, vpeer}}
  end

  def teardown_untrusted({upeer = {unode, _}, vpeer = {vnode, _}}) do
    @peers.disable_untrusted(upeer)
    @peers.disable_untrusted(vpeer)

    assert(match?(:pong, @peers.rpc(upeer, Node, :ping, [vnode])))
    assert(match?(:pong, @peers.rpc(vpeer, Node, :ping, [unode])))

    assert([] === @peers.logger_export(upeer))
    assert([] === @peers.logger_export(vpeer))
    assert([] === @peers.handler_export(upeer))
    assert([] === @peers.handler_export(vpeer))

    :ok = @peers.teardown({upeer, vpeer})
  end

  def __after_compile__(env, bytecode) do
    _ = :persistent_term.put(__MODULE__, {__MODULE__, bytecode, :unicode.characters_to_list(env.file)})
    :ok
  end
end
