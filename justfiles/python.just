# Copyright (c) Meta Platforms, Inc. and affiliates.
# Copyright (c) WhatsApp LLC
#
# This source code is licensed under the MIT license found in the
# LICENSE.md file in the root directory of this source tree.

[linux]
setup-python:
    # Use the latest available pip
    pip install --upgrade pip
    # Use the latest available poetry
    pip install --upgrade poetry
    # Setup poetry export plugin
    poetry self add poetry-plugin-export
    # Ensure that virtualenvs are installed in .venv
    poetry config virtualenvs.in-project true
    # Setup poetry shell plugin
    poetry self add poetry-plugin-shell

[linux]
[working-directory: '/project/codegen']
poetry-install:
    poetry install --no-interaction

[linux]
[working-directory: '/project/codegen']
poetry-lock:
    poetry lock --no-interaction
    poetry install --no-interaction
    (poetry export --no-interaction --without-hashes --without-urls --format=requirements.txt | awk '{ print $1 }' FS=';' | awk '{$1=$1};1') > requirements.txt
