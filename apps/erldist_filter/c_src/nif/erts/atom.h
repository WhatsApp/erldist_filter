/*
 * %CopyrightBegin%
 *
 * SPDX-License-Identifier: Apache-2.0
 *
 * Copyright Ericsson AB 1996-2025. All Rights Reserved.
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * Copyright (c) WhatsApp LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * %CopyrightEnd%
 */

#ifndef EDF_ERTS_ATOM_H
#define EDF_ERTS_ATOM_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>

#include "../erl_nif_trampoline.h"

/// See
/// [erts/emulator/beam/atom.h](https://github.com/erlang/otp/blob/OTP-28.0.2/erts/emulator/beam/atom.h)
/// in the Erlang/OTP source code.

#define MAX_ATOM_CHARACTERS 255
#define MAX_ATOM_SZ_FROM_LATIN1 (2 * MAX_ATOM_CHARACTERS)
#define MAX_ATOM_SZ_LIMIT (4 * MAX_ATOM_CHARACTERS) /* theoretical byte limit */

#ifdef __cplusplus
}
#endif

#endif
