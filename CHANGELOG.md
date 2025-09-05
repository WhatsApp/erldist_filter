# Changelog

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
