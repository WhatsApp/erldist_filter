# Copyright (c) Meta Platforms, Inc. and affiliates.
# Copyright (c) WhatsApp LLC
#
# This source code is licensed under the MIT license found in the
# LICENSE.md file in the root directory of this source tree.

docker_image := "erlang-edf-codegen"

[macos]
_docker-exec +args: docker-build
    docker run --rm --volume $(pwd):/project {{docker_image}} {{args}}

[macos]
_docker-interactive +args: docker-build
    docker run --rm --volume $(pwd):/project -it {{docker_image}} {{args}}

[group('docker')]
[macos]
docker-build:
    docker build -f ./codegen/Dockerfile -t {{docker_image}} .

[group('docker')]
[macos]
docker-codegen:
    @just _docker-exec just codegen

[group('docker')]
[macos]
docker-format:
    @just _docker-exec just format

[group('docker')]
[macos]
docker-lock:
    @just _docker-exec just poetry-lock

[group('docker')]
[macos]
docker-shell:
    @just _docker-interactive /bin/bash
