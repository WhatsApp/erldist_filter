# `erldist_filter`

[![Build Status](https://github.com/WhatsApp/erldist_filter/actions/workflows/ci.yml/badge.svg?branch=main)](https://github.com/WhatsApp/erldist_filter/actions) [![Hex.pm](https://img.shields.io/hexpm/v/erldist_filter.svg)](https://hex.pm/packages/erldist_filter)

`erldist_filter` NIF for filtering and logging [Erlang Dist Protocol](https://www.erlang.org/doc/apps/erts/erl_dist_protocol.html) messages.

See the [CONTRIBUTING](CONTRIBUTING.md) file for how to help out.

## Installation

Add `erldist_filter` to your project's dependencies in `mix.exs`

```elixir
defp deps() do
  [
    {:erldist_filter, "~> 1.28"}
  ]
end
```

Add `erldist_filter` to your project's dependencies in your `Makefile` for [`erlang.mk`](https://github.com/ninenines/erlang.mk) or the following to your `rebar.config`

```erlang
{deps, [
    {erldist_filter, "~> 1.28"}
]}.
```

## License

`erldist_filter` is MIT licensed, as found in the [LICENSE](LICENSE.md) file.
