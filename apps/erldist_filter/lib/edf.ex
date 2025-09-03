# Copyright (c) Meta Platforms, Inc. and affiliates.
# Copyright (c) WhatsApp LLC
#
# This source code is licensed under the MIT license found in the
# LICENSE.md file in the root directory of this source tree.

defmodule EDF do
  require Record

  records =
    :lists.usort(
      EDF.RecordExtractor.extract_all(from_lib: "erldist_filter/include/erldist_filter.hrl") ++
        EDF.RecordExtractor.extract_all(from_lib: "erldist_filter/include/udist.hrl")
    )

  metadata =
    for {record, %{fields: fields}} <- records do
      Record.defrecord(record, fields)
      %{kind: :defrecord, fields: :lists.map(&elem(&1, 0), fields), tag: record, name: record}
    end

  [record_guards0, record_guards1 | record_guards_rest] =
    for {record, %{fields: fields}} <- records, into: [] do
      record_size = length(fields) + 1

      quote do
        Record.is_record(var!(data), unquote(record)) and tuple_size(var!(data)) === unquote(record_size)
      end
    end

  record_guards_init =
    quote do
      unquote(record_guards0) or unquote(record_guards1)
    end

  record_guards =
    :lists.foldl(
      fn record_guard, acc ->
        quote do
          unquote(acc) or unquote(record_guard)
        end
      end,
      record_guards_init,
      record_guards_rest
    )

  defguardp _is_record_type(data) when unquote(record_guards)

  for {record, %{types: types}} <- records, into: [] do
    @type unquote(record)() :: record(unquote(record), unquote(types))
  end

  [record_types0, record_types1 | record_types_rest] =
    for {record, _definition} <- :lists.reverse(records), into: [] do
      quote do
        unquote(record)()
      end
    end

  record_types_init =
    quote do
      unquote(record_types1) | unquote(record_types0)
    end

  record_types =
    :lists.foldl(
      fn record_type, acc ->
        quote do
          unquote(record_type) | unquote(acc)
        end
      end,
      record_types_init,
      record_types_rest
    )

  @type t() :: unquote(record_types)

  defmodule Pretty do
    @type t() :: %__MODULE__{
            name: atom(),
            pairs: Keyword.t()
          }

    @enforce_keys [:name, :pairs]
    defstruct [:name, :pairs]

    for {record, %{fields: fields}} <- records do
      keys = :lists.map(&elem(&1, 0), fields)
      vals = :lists.map(&{&1, [], nil}, keys)
      pairs = :lists.zip(keys, vals)

      def cast({unquote(record), unquote_splicing(vals)}) do
        %__MODULE__{name: unquote(record), pairs: [unquote_splicing(pairs)]}
      end
    end

    @spec inspect(term(), opts :: Inspect.Opts.t()) :: Inspect.Algebra.t()
    def inspect(term, opts = %Inspect.Opts{}) do
      {opts, inspect_fun} = inspect_opts(opts)

      if EDF.is_record_type(term) do
        inspect_fun.(cast(term), opts)
      else
        inspect_fun.(term, opts)
      end
    end

    @spec inspect_opts(opts) :: {opts, (term(), opts -> Inspect.Algebra.t())} when opts: Inspect.Opts.t()
    def inspect_opts(opts = %Inspect.Opts{}) do
      case Keyword.get(opts.custom_options, __MODULE__) do
        nil ->
          inspect_fun = maybe_break_infinite_loop(opts.inspect_fun)

          {%{
             opts
             | custom_options: Keyword.put(opts.custom_options, __MODULE__, inspect_fun),
               inspect_fun: &__MODULE__.inspect/2
           }, inspect_fun}

        inspect_fun when is_function(inspect_fun, 2) ->
          {opts, maybe_break_infinite_loop(inspect_fun)}
      end
    end

    defp maybe_break_infinite_loop(inspect_fun) when is_function(inspect_fun, 2) do
      case &__MODULE__.inspect/2 do
        ^inspect_fun ->
          Inspect.Opts.default_inspect_fun()

        _ ->
          inspect_fun
      end
    end
  end

  defimpl Inspect, for: Pretty do
    @impl true
    def inspect(%Pretty{name: name, pairs: pairs}, opts) do
      open = Inspect.Algebra.color("EDF." <> to_string(name) <> "(", :map, opts)
      sep = Inspect.Algebra.color(",", :map, opts)
      close = Inspect.Algebra.color(")", :map, opts)
      fun = &Inspect.List.keyword/2
      Inspect.Algebra.container_doc(open, pairs, close, opts, fun, separator: sep, break: :strict)
    end
  end

  @spec inspect(Inspect.t(), keyword()) :: String.t()
  def inspect(term, opts \\ []) do
    {opts, _inspect_fun} = Pretty.inspect_opts(Inspect.Opts.new(opts))
    opts = Map.to_list(Map.from_struct(opts))
    Kernel.inspect(term, opts)
  end

  @spec is_record_type(term()) :: boolean()
  def is_record_type(term) when _is_record_type(term) do
    true
  end

  def is_record_type(_) do
    false
  end

  @spec record_definitions :: [%{fields: [atom()], kind: :defrecord, name: atom(), tag: atom()}, ...]
  def record_definitions() do
    unquote(Macro.escape(metadata))
  end
end
