# Changelog

## 1.29.1 (2026-10-01)

* Distribution
  * Keep TCP sockets passive until the receiver owns them, preventing connection loss during handoff.
  * Make the channel the sole owner of receive-path externals and safely handle teardown during receives [#8](https://github.com/WhatsApp/erldist_filter/pull/8).
* Testing and maintenance
  * Test the latest OTP 28/29 releases, 28.5.0.7 and 29.1.1; use OTP 29.1.1 for sanitizers.
  * Refresh Elixir/Mix, Rebar3, ELP/eqwalizer, elixir_make, Erlang.mk, and Python dependencies.
  * Preserve configuration field types and validate distribution-operation fields with the current eqwalizer; reuse constructor validation and test raw/record round trips.
  * Make eqwalizer type errors fail CI.
  * Pin PropEr's upstream OTP 29 fixes and remove deprecated Mix configuration warnings.
  * Expand map and record generators and add receive-teardown, connection-handoff, and priority-message regressions.
  * Restore Erlang/C formatting checks in CI with clang-format 22.1.8, preserving the existing label layout; regenerate and format sources.
  * Make Linux codegen recipes work from any checkout and preserve Python dependency environment markers.
  * Fix C++ warning and sanitizer flag propagation.

## 1.29.0 (2026-07-27)

* Erlang/OTP support
  * Track OTP 28/29; refresh upstream imports and CI.
  * Support native records (`DFLAG_NATIVE_RECORDS` and `RECORD_EXT`) [#7](https://github.com/WhatsApp/erldist_filter/pull/7)
  * Use OTP 29's public atom-cache-index NIF API.
* Distribution decoding
  * Restore cached-atom term decoding with property coverage [#6](https://github.com/WhatsApp/erldist_filter/pull/6)
  * Fix unsafe NIF cleanup for distribution messages without payloads.
* Testing and maintenance
  * Add `just sanitizers` for ASan, LSan, UBSan, and cleanup stress.
  * Add `just cover` for Common Test coverage reports.
  * Make generated-source signing idempotent.
  * Add `MAINTENANCE.md` and `AGENTS.md`; refresh ELP and eqwalizer.
  * Fixed flaky SPBT tests.
* Security
  * Added 15 registered processes to the blocklist.
  * Block high-risk OTP execution, code-loading, and callback-installation message shapes.

## 1.28.5 (2026-05-11)

* Minor fix: replace deprecated bare `catch Expr` with `try ... catch ... end` [#5](https://github.com/WhatsApp/erldist_filter/pull/5)
* Maintenance
  * Bump codegen deps
  * Bump CI versions tested for Erlang/OTP to 28.5 and 27.3.4.11

## 1.28.3 (2025-09-05)

* Minor fix: remove unnecessary codesigning.

## 1.28.2 (2025-09-05)

* Minor fix: codesigning and formatting.

## 1.28.1 (2025-09-05)

* Use stricter `CFLAGS` and `CXXFLAGS` for warnings.
  * Fix warnings exposed.
* Use codegen version instead of unstable builtin macros.
* Detect whether `clang` is being used and add warning flags if applicable.
* Update [`elp` (erlang-language-platform) to 2025-09-01](https://github.com/WhatsApp/erlang-language-platform/releases/tag/2025-09-01).
* Fix typing errors found from `make lint`.

## 1.28.0 (2025-09-04)

* Add support for Erlang/OTP 27 and 28.
* Support `DOP_ALTACT_SIG_SEND` dist operation.
* Add `otp_name_blocklist` configuration option to block OTP named processes.
* Fix 0-byte tick crash when connection is idle.
* Improve test reliability.

## 1.1.0 (2023-10-04)

* Added "fastpath" and "slowpath" branching for faster filtering of smaller messages.
* I/O request and reply are dropped by default now (may be allowed by enabled `untrusted` mode with a custom handler).

## 1.0.0 (2023-09-07)

* Initial release.
