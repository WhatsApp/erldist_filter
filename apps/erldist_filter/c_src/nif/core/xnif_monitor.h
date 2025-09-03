/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * Copyright (c) WhatsApp LLC
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE.md file in the root directory of this source tree.
 */

#ifndef XNIF_MONITOR_H
#define XNIF_MONITOR_H

#ifdef __cplusplus
extern "C" {
#endif

#include <assert.h>
#include <inttypes.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>

#include "../erl_nif_trampoline.h"

/* Macro Definitions */

/* Type Definitions */

typedef struct xnif_monitor_s xnif_monitor_t;

struct xnif_monitor_s {
    ErlNifPid pid;
    ErlNifMonitor monitor;
};

/* Function Declarations */

static void xnif_monitor_set_undefined(xnif_monitor_t *mon);
static bool xnif_monitor_is_undefined(const xnif_monitor_t *mon);
static int xnif_monitor_self(ErlNifEnv *env, void *obj, xnif_monitor_t *mon);
static int xnif_monitor_process(ErlNifEnv *env, void *obj, const ErlNifPid *target_pid, xnif_monitor_t *mon);
static int xnif_demonitor_process(ErlNifEnv *env, void *obj, xnif_monitor_t *mon);

/* Inline Function Definitions */

inline void
xnif_monitor_set_undefined(xnif_monitor_t *mon)
{
    (void)enif_set_pid_undefined(&mon->pid);
    (void)memset(&mon->monitor, 0, sizeof(ErlNifMonitor));
    return;
}

inline bool
xnif_monitor_is_undefined(const xnif_monitor_t *mon)
{
    return enif_is_pid_undefined(&mon->pid);
}

inline int
xnif_monitor_self(ErlNifEnv *env, void *obj, xnif_monitor_t *mon)
{
    ErlNifPid target_pid;
    if (enif_self(env, &target_pid) == NULL) {
        return 1;
    }
    return xnif_monitor_process(env, obj, &target_pid, mon);
}

inline int
xnif_monitor_process(ErlNifEnv *env, void *obj, const ErlNifPid *target_pid, xnif_monitor_t *mon)
{
    int retval;
    if (!xnif_monitor_is_undefined(mon)) {
        (void)xnif_demonitor_process(env, obj, mon);
    }
    retval = enif_monitor_process(env, (void *)obj, target_pid, &mon->monitor);
    if (retval < 0) {
        return retval;
    } else if (retval > 0) {
        return retval;
    }
    mon->pid = *target_pid;
    return retval;
}

inline int
xnif_demonitor_process(ErlNifEnv *env, void *obj, xnif_monitor_t *mon)
{
    int retval;
    retval = enif_demonitor_process(env, obj, &mon->monitor);
    (void)xnif_monitor_set_undefined(mon);
    return retval;
}

#ifdef __cplusplus
}
#endif

#endif
