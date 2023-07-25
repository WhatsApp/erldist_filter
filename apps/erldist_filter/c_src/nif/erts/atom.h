/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2015-2021. All Rights Reserved.
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

#include <erl_nif.h>

/// See
/// [erts/emulator/beam/atom.h](https://github.com/erlang/otp/blob/OTP-25.2.3/erts/emulator/beam/atom.h)
/// in the Erlang/OTP source code.

#define MAX_ATOM_CHARACTERS 255
#define MAX_ATOM_SZ_FROM_LATIN1 (2 * MAX_ATOM_CHARACTERS)
#define MAX_ATOM_SZ_LIMIT (4 * MAX_ATOM_CHARACTERS) /* theoretical byte limit */

typedef enum {
    ERTS_ATOM_ENC_7BIT_ASCII,
    ERTS_ATOM_ENC_LATIN1,
    ERTS_ATOM_ENC_UTF8,
} ErtsAtomEncoding;

extern ERL_NIF_TERM erts_atom_put(const uint8_t *name, signed int len, ErtsAtomEncoding enc, int trunc);

#ifdef __cplusplus
}
#endif

#endif
