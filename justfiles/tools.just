# Copyright (c) Meta Platforms, Inc. and affiliates.
# Copyright (c) WhatsApp LLC
#
# This source code is licensed under the MIT license found in the
# LICENSE.md file in the root directory of this source tree.

sanitizers_image := "erldist-filter-sanitizers"

[linux]
[working-directory: justfile_directory() / 'codegen']
codegen: poetry-install format
    poetry run erlang_edf_codegen json-schema schema.json
    poetry run erlang_edf_codegen generate config.yaml .. templates0 templates1
    just format-c-targets

[macos]
codegen: docker-codegen
    gmake format
    just sign

[linux]
format: poetry-install format-c-targets format-c-templates format-python-src

[linux]
[working-directory: justfile_directory() / 'codegen']
format-c-targets:
    #!/usr/bin/env bash
    set -euo pipefail
    poetry run clang-format --version
    find ../apps/erldist_filter/c_src \( -type l -o -type f \) \( -name '*.c' -o -name '*.C' -o -name '*.cc' -o -name '*.cpp' -o -name '*.h' -o -name '*.c.h' -o -name '*.hpp' \) -print0 | \
        xargs -0 poetry run clang-format --style="file:../apps/erldist_filter/c_src/.clang-format" -i

[linux]
[working-directory: justfile_directory() / 'codegen']
format-c-templates:
    #!/usr/bin/env bash
    set -euo pipefail
    poetry run clang-format --version
    find templates0 templates1 \( -type l -o -type f \) \( -name '*.c.j2' -o -name '*.C.j2' -o -name '*.cc.j2' -o -name '*.cpp.j2' -o -name '*.h.j2' -o -name '*.c.h.j2' -o -name '*.hpp.j2' \) -print0 | \
        xargs -0 poetry run clang-format --style="file:../apps/erldist_filter/c_src/.clang-format" -i

[linux]
[working-directory: justfile_directory() / 'codegen']
format-python-src:
    poetry run black src/ tests/

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
[working-directory: justfile_directory() / 'codegen']
sign: poetry-install
    poetry run erlang_edf_codegen sign config.yaml .. templates0 templates1

[macos]
sign: docker-sign

[macos]
watch:
    watchexec -w codegen -- just codegen

[group('test')]
cover:
    rebar3 ct --cover
    rebar3 cover

[group('test')]
sanitizers: sanitizers-docker

[group('test')]
[linux]
sanitizers-native:
    bash "{{justfile_directory()}}/justfiles/sanitizers/run.sh" "{{justfile_directory()}}"

[group('test')]
sanitizers-docker: sanitizers-docker-build
    docker run --rm {{sanitizers_image}}

[group('docker')]
sanitizers-docker-build:
    docker build --file justfiles/sanitizers/Dockerfile --tag {{sanitizers_image}} .
