# Maintenance

Keep only the two newest OTP majors. Run from the repository root.

## Routine

- Update all dependencies under `codegen/`; refresh its lock and requirements files (`just lock`), then run `just codegen`.
- Bump the ELP/eqwalizer (`ELP_VERSION`, `ELP_OTP_VERSION`) and erlfmt (`ERLFMT_VERSION`) pins in `Makefile`; run `gmake distclean-elp distclean-erlfmt` so fresh tools are downloaded.
- Use the latest releases of only the two newest OTP majors; update OTP, Elixir, and Rebar3 pins repo-wide, especially in CI and every Dockerfile. Test both majors.
- Run `just codegen`, `gmake format`, `gmake lint`, `rebar3 ct`, `mix test`, and `just sanitizers`; review generated changes, then run `git diff --check`.
- Run `just cover`; inspect `_build/test/cover/index.html` and add tests for meaningful gaps.

## OTP Version Change

- Rebase local patches onto matching upstream `maint-N`: `apps/erldist_filter/include/erldist_filter_erts_{dist,external}.hrl`, all of `apps/erldist_filter/c_src/nif/erts/`, and every `apps/erldist_filter/{include,src}/erldist_filter_otp_N_*` file. Preserve local changes and update source links; for a major change, add the newest set and delete the oldest.
- Inventory imports and branches repo-wide with `rg -l 'erlang/otp' .` and `rg -n 'OTP_RELEASE|[Oo][Tt][Pp][_-]?[0-9]{2}' .`; update all dispatch modules/tests and remove obsolete gates. Edit generator inputs, never `DO NOT EDIT` outputs.
- Review new features and breaking changes in [distribution](https://www.erlang.org/doc/apps/erts/erl_dist_protocol.html), [ETF](https://www.erlang.org/doc/apps/erts/erl_ext_dist.html), and [NIF](https://www.erlang.org/doc/apps/erts/erl_nif.html).
- Map new DFLAGs. For every new/changed DOP, define `vdist` and `udist` across Erlang, C, codegen types, property tests, and `vedf` tests.
- For every new ETF type/tag, define `vterm` across Erlang, C, codegen types, and property tests. Adopt applicable NIF API changes.
- Run the routine checklist on both OTP majors; new behavior is incomplete without property/`vedf` coverage.

## Defensive Review

- For each OTP major, record the upstream revision and statically review registered/global handlers for code loading, OS processes, callbacks, file writes, and runtime control. Do not build payloads or runtime-test attacks.
- Classify candidates as confirmed, defense-in-depth, or rejected. Record the minimal selector, OTP source/function, behavior and coverage gap, control, compatibility risk, and confidence.
- Add names through `codegen/config.yaml`; account for aliases and global-name-to-PID resolution. Implement exact structural matches in both `udist.c` and `vedf_channel.erl`.
- Add positive and near-miss generator tests; regenerate, clean-rebuild after atom changes, and exercise every traffic/action mode. Run routine checks and update `CHANGELOG.md`.
