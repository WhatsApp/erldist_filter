# Copyright (c) Meta Platforms, Inc. and affiliates.
# Copyright (c) WhatsApp LLC
#
# This source code is licensed under the MIT license found in the
# LICENSE.md file in the root directory of this source tree.

from types import SimpleNamespace

from erlang_edf_codegen.code_generator import TOKEN, StageTarget


def test_sign_is_idempotent(tmp_path):
    stage = SimpleNamespace(codegen=SimpleNamespace(output_path=str(tmp_path)))
    target = StageTarget(stage=stage, template_name="generated.c.j2", output_name="generated.c")
    unsigned = f"// @generated {TOKEN}\nint generated(void) {{ return 0; }}\n".encode("utf-8")
    output_file = tmp_path / "generated.c"
    output_file.write_bytes(unsigned)

    target.sign()
    signed_once = output_file.read_bytes()
    target.sign()

    assert output_file.read_bytes() == signed_once
    assert TOKEN.encode("utf-8") not in signed_once
