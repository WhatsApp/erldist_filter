/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * Copyright (c) WhatsApp LLC
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE.md file in the root directory of this source tree.
 */

#ifndef EDF_MPID_H
#define EDF_MPID_H

#ifdef __cplusplus
extern "C" {
#endif

#include "edf_common.h"

/* Macro Definitions */

/* Type Definitions */

typedef struct edf_mpid_s edf_mpid_t;

struct edf_mpid_s {
    ErlNifPid pid;
    ErlNifMonitor mon;
};

/* Function Declarations */

static void edf_mpid_set_undefined(edf_mpid_t *mpid);
static bool edf_mpid_is_undefined(const edf_mpid_t *mpid);
static int edf_mpid_monitor_self(ErlNifEnv *env, void *obj, edf_mpid_t *mpid);
static int edf_mpid_monitor_process(ErlNifEnv *env, void *obj, const ErlNifPid *target_pid, edf_mpid_t *mpid);
static int edf_mpid_demonitor_process(ErlNifEnv *env, void *obj, edf_mpid_t *mpid);

/* Inline Function Definitions */

inline void
edf_mpid_set_undefined(edf_mpid_t *mpid)
{
    (void)enif_set_pid_undefined(&mpid->pid);
    (void)memset(&mpid->mon, 0, sizeof(ErlNifMonitor));
}

inline bool
edf_mpid_is_undefined(const edf_mpid_t *mpid)
{
    return enif_is_pid_undefined(&mpid->pid);
}

inline int
edf_mpid_monitor_self(ErlNifEnv *env, void *obj, edf_mpid_t *mpid)
{
    ErlNifPid target_pid;
    if (enif_self(env, &target_pid) == NULL) {
        // result_err(result)->term = EXCP_ERROR(env, "Call to enif_self() failed: not a process bound environment");
        // (void)result->Err(EXCP_ERROR(env, "Call to enif_self() failed: not a process bound environment"));
        return 1;
    }
    return edf_mpid_monitor_process(env, obj, &target_pid, mpid);
}

inline int
edf_mpid_monitor_process(ErlNifEnv *env, void *obj, const ErlNifPid *target_pid, edf_mpid_t *mpid)
{
    int retval;
    if (!edf_mpid_is_undefined(mpid)) {
        (void)edf_mpid_demonitor_process(env, obj, mpid);
    }
    retval = enif_monitor_process(env, (void *)obj, target_pid, &mpid->mon);
    if (retval < 0) {
        // return EXCP_ERROR(env, "Call to enif_monitor_process() failed: no `down' callback provided");
        return retval;
    } else if (retval > 0) {
        // (void)enif_release_resource((void *)resource);
        // return EXCP_ERROR(env, "Call to enif_monitor_process() failed: target process is no longer alive");
        return retval;
    }
    mpid->pid = *target_pid;
    return retval;
}

inline int
edf_mpid_demonitor_process(ErlNifEnv *env, void *obj, edf_mpid_t *mpid)
{
    int retval;
    retval = enif_demonitor_process(env, obj, &mpid->mon);
    (void)edf_mpid_set_undefined(mpid);
    return retval;
}

#ifdef __cplusplus
}
#endif

#endif
