defmodule EDF.RecordExtractor do
  @moduledoc false

  def extract(name, opts) do
    extract_record(name, from_or_from_lib_file(opts))
  end

  def extract_all(opts) do
    extract_all_records(from_or_from_lib_file(opts))
  end

  defp from_or_from_lib_file(opts) do
    cond do
      file = opts[:from] ->
        {from_file(file), Keyword.delete(opts, :from)}

      file = opts[:from_lib] ->
        {from_lib_file(file), Keyword.delete(opts, :from_lib)}

      true ->
        raise ArgumentError, "expected :from or :from_lib to be given as option"
    end
  end

  # Find file using the same lookup as the *include* attribute from Erlang modules.
  defp from_file(file) do
    file = String.to_charlist(file)

    case :code.where_is_file(file) do
      :non_existing -> file
      realfile -> realfile
    end
  end

  # Find file using the same lookup as the *include_lib* attribute from Erlang modules.
  defp from_lib_file(file) do
    [app | path] = :filename.split(String.to_charlist(file))

    case :code.lib_dir(List.to_atom(app)) do
      {:error, _} ->
        raise ArgumentError, "lib file #{file} could not be found"

      libpath ->
        :filename.join([libpath | path])
    end
  end

  # Retrieve the record with the given name from the given file
  defp extract_record(name, {file, opts}) do
    form = read_file(file, opts)
    records = extract_records(form)

    if record = List.keyfind(records, name, 0) do
      parse_record(record, form)
    else
      raise ArgumentError,
            "no record #{name} found at #{file}. Or the record does not exist or " <>
              "its entry is malformed or depends on other include files"
    end
  end

  # Retrieve all records from the given file
  defp extract_all_records({file, opts}) do
    form = read_file(file, opts)
    records = extract_records(form)
    for rec = {name, _fields} <- records, do: {name, parse_record(rec, form)}
  end

  # Parse the given file and extract all existent records.
  defp extract_records(form) do
    for {:attribute, _, :record, record} <- form, do: record
  end

  # Read a file and return its abstract syntax form that also
  # includes record but with macros and other attributes expanded,
  # such as "-include(...)" and "-include_lib(...)". This is done
  # by using Erlang's epp.
  defp read_file(file, opts) do
    case :epp.parse_file(file, opts) do
      {:ok, form} ->
        form

      other ->
        raise "error parsing file #{file}, got: #{inspect(other)}"
    end
  end

  # Parse a tuple with name and fields and returns a
  # list of tuples where the first element is the field
  # and the second is its default value.
  defp parse_record({_name, fields}, form) do
    {cons, types} =
      List.foldr(fields, {{nil, 0}, []}, fn f, {acc, types} ->
        {{:cons, 0, parse_field(f), acc}, [parse_field_typing(f) | types]}
      end)

    %{
      fields: eval_record(cons, form),
      types: types
    }
  end

  defp parse_field({:typed_record_field, record_field, _type}) do
    parse_field(record_field)
  end

  defp parse_field({:record_field, _, key}) do
    {:tuple, 0, [key, {:atom, 0, :undefined}]}
  end

  defp parse_field({:record_field, _, key, value}) do
    {:tuple, 0, [key, value]}
  end

  defp parse_field_typing({:typed_record_field, record_field, type}) do
    {key, _default_type} = parse_field_typing(record_field)
    {key, parse_field_typing_type(type)}
  end

  defp parse_field_typing({:record_field, _, key}) do
    {parse_field_typing_key(key), parse_field_typing_type({:type, 0, :term, []})}
  end

  defp parse_field_typing({:record_field, _, key, _value}) do
    {parse_field_typing_key(key), parse_field_typing_type({:type, 0, :term, []})}
  end

  defp parse_field_typing_key({:atom, _, key}) do
    key
  end

  defp parse_field_typing_type({:type, _, type, []}) when is_atom(type) do
    quote do
      unquote(type)()
    end
  end

  defp parse_field_typing_type({:type, _, :union, types = [_, _ | _]}) do
    [types0, types1 | types_rest] =
      List.foldl(types, [], fn type, acc ->
        [parse_field_typing_type(type) | acc]
      end)

    types_init =
      quote do
        unquote(types1) | unquote(types0)
      end

    :lists.foldl(
      fn type, acc ->
        quote do
          unquote(type) | unquote(acc)
        end
      end,
      types_init,
      types_rest
    )
  end

  defp parse_field_typing_type({:atom, _, atom}) do
    quote do
      unquote(atom)
    end
  end

  defp parse_field_typing_type({:integer, _, integer}) do
    quote do
      unquote(integer)
    end
  end

  defp parse_field_typing_type({:type, _, :binary, [{:integer, _, bits}, {:integer, _, 0}]}) do
    quote do
      <<_::unquote(bits)>>
    end
  end

  defp parse_field_typing_type({:type, _, :list, [type]}) do
    type = parse_field_typing_type(type)

    quote do
      [unquote(type)]
    end
  end

  defp parse_field_typing_type({:type, _, :map, map_fields}) do
    map_fields =
      for map_field <- map_fields, into: [] do
        parse_field_typing_type(map_field)
      end

    quote do
      %{unquote_splicing(map_fields)}
    end
  end

  defp parse_field_typing_type({:type, _, :map_field_assoc, [key, value]}) do
    key = parse_field_typing_type(key)
    value = parse_field_typing_type(value)

    {:%{}, _, [pair]} =
      quote do
        %{optional(unquote(key)) => unquote(value)}
      end

    pair
  end

  defp parse_field_typing_type({:type, _, :map_field_exact, [key, value]}) do
    key = parse_field_typing_type(key)
    value = parse_field_typing_type(value)

    {:%{}, _, [pair]} =
      quote do
        %{required(unquote(key)) => unquote(value)}
      end

    pair
  end

  defp parse_field_typing_type({:type, _, :tuple, types}) when is_list(types) do
    types =
      for type <- types, into: [] do
        parse_field_typing_type(type)
      end

    quote do
      {unquote_splicing(types)}
    end
  end

  defp parse_field_typing_type({:type, _, :range, [{:integer, _, lo}, {:integer, _, hi}]}) do
    quote do
      unquote(lo)..unquote(hi)
    end
  end

  defp parse_field_typing_type({:remote_type, _, [{:atom, _, module}, {:atom, _, type}, args]}) do
    args =
      for arg <- args, into: [] do
        parse_field_typing_type(arg)
      end

    quote do
      unquote(module).unquote(type)(unquote_splicing(args))
    end
  end

  defp eval_record(cons, form) do
    form = form ++ [{:function, 0, :hello, 0, [{:clause, 0, [], [], [cons]}]}]

    {:function, 0, :hello, 0, [{:clause, 0, [], [], [record_ast]}]} =
      :erl_expand_records.module(form, []) |> List.last()

    {:value, record, _} = :erl_eval.expr(record_ast, [])
    record
  end
end
