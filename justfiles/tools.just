# Copyright (c) Meta Platforms, Inc. and affiliates.
# Copyright (c) WhatsApp LLC
#
# This source code is licensed under the MIT license found in the
# LICENSE.md file in the root directory of this source tree.

[linux]
[working-directory: '/project/codegen']
codegen: poetry-install format
    poetry run erlang_edf_codegen json-schema /project/codegen/schema.json
    poetry run erlang_edf_codegen generate /project/codegen/config.yaml /project /project/codegen/templates0 /project/codegen/templates1
    just format-c-targets

[macos]
codegen: docker-codegen
    gmake format
    just sign

[linux]
format: format-c-targets format-c-templates format-python-src

[linux]
format-c-targets:
    #!/usr/bin/env bash
    files=$(find /project/apps/erldist_filter/c_src \( -type l -o -type f \) \( -name '*.c' -o -name '*.C' -o -name '*.cc' -o -name '*.cpp' -o -name '*.h' -o -name '*.c.h' -o -name '*.hpp' \) | grep -v " ")
    # set -x
    clang-format --version
    exec clang-format --style="file:/project/apps/erldist_filter/c_src/.clang-format" -i ${files}

[linux]
format-c-templates:
    #!/usr/bin/env bash
    files=$(find /project/codegen/{templates0,templates1} \( -type l -o -type f \) \( -name '*.c.j2' -o -name '*.C.j2' -o -name '*.cc.j2' -o -name '*.cpp.j2' -o -name '*.h.j2' -o -name '*.c.h.j2' -o -name '*.hpp.j2' \) | grep -v " ")
    # set -x
    clang-format --version
    exec clang-format --style="file:/project/apps/erldist_filter/c_src/.clang-format" -i ${files}

[linux]
[working-directory: '/project/codegen']
format-python-src:
    poetry run black /project/codegen/src/

[macos]
format: docker-format
    gmake format

[linux]
lock:
    just poetry-lock

[macos]
lock: docker-lock

[macos]
shell: docker-shell

[linux]
[working-directory: '/project/codegen']
sign: poetry-install
    poetry run erlang_edf_codegen sign /project/codegen/config.yaml /project /project/codegen/templates0 /project/codegen/templates1

[macos]
sign: docker-sign

[macos]
watch:
    watchexec -w codegen -- just codegen
