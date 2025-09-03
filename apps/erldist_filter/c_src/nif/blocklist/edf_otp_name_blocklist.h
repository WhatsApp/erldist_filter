/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * Copyright (c) WhatsApp LLC
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE.md file in the root directory of this source tree.
 */

#ifndef EDF_OTP_NAME_BLOCKLIST_H
#define EDF_OTP_NAME_BLOCKLIST_H

#ifdef __cplusplus
extern "C" {
#endif

#include "../erldist_filter_nif.h"

/* Macro Definitions */

/* Type Definitions */

typedef struct edf_otp_name_blocklist_s edf_otp_name_blocklist_t;

/* Global Declarations */

extern edf_otp_name_blocklist_t *edf_otp_name_blocklist;

/* Function Declarations */

extern int edf_otp_name_blocklist_load(ErlNifEnv *env);
extern void edf_otp_name_blocklist_unload(ErlNifEnv *env);
extern bool edf_otp_name_is_blocked(ERL_NIF_TERM name);
extern ERL_NIF_TERM erldist_filter_nif_otp_name_blocklist_0(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM erldist_filter_nif_otp_name_is_blocked_1(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);

/* Inline Function Definitions */

#ifdef __cplusplus
}
#endif

#endif
