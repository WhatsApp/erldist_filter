# Agent Notes

- Erlang app: `apps/erldist_filter`; tests: `apps/erldist_filter_test`; NIF: `apps/erldist_filter/c_src/nif`.
- Generated files say `DO NOT EDIT`. Change `codegen/config.yaml`, `codegen/schema.json`, or `codegen/templates{0,1}`, then run `just codegen`.
- Validate with `just format`, `gmake lint`, `rebar3 ct`, and `git diff --check`.
- NIF-only build: `gmake -C apps/erldist_filter/c_src` (`make` on Linux).
- Sanitizers: `just sanitizers` runs ASan, LSan, and UBSan in Docker with an ASan-built OTP; Linux may use `just sanitizers-native` when `beam.asan` is installed.
- For routine or OTP-version maintenance, follow `MAINTENANCE.md`.
