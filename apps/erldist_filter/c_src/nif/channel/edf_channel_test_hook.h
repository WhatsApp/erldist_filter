/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * Copyright (c) WhatsApp LLC
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE.md file in the root directory of this source tree.
 */

#ifndef EDF_CHANNEL_TEST_HOOK_H
#define EDF_CHANNEL_TEST_HOOK_H

#ifdef __cplusplus
extern "C" {
#endif

#include "edf_channel.h"

/*
 * Test-only instrumentation for channel teardown and the receive trap, used by
 * erldist_filter_channel_teardown_SUITE. Only compiled with EDF_TEST_HOOKS=1.
 *
 * Events and the receive barrier only apply to the single channel resource passed
 * to erldist_filter_nif:test_hook_arm/3. Every event is written to stderr (so the
 * order survives a sanitizer abort) and sent to the armed notify pid.
 *
 * The receive barrier never blocks: while it is armed and the in-flight external
 * matches, the receive trap returns TRAP_YIELD() without making progress, which
 * releases the channel lock and reschedules the trap until the barrier is opened.
 */

#ifdef EDF_TEST_HOOKS

extern void edf_channel_test_hook_event(const char *name, const edf_channel_resource_t *resource, const void *trap,
                                        const void *external);
extern int edf_channel_test_hook_recv_barrier(const edf_channel_resource_t *resource, const void *trap, edf_external_t *external);

extern ERL_NIF_TERM erldist_filter_nif_test_hook_arm_3(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM erldist_filter_nif_test_hook_open_0(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);
extern ERL_NIF_TERM erldist_filter_nif_test_hook_disarm_0(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]);

#define EDF_CHANNEL_TEST_HOOK_EVENT(name, resource, trap, external)                                                                \
    edf_channel_test_hook_event((name), (resource), (const void *)(trap), (const void *)(external))
#define EDF_CHANNEL_TEST_HOOK_RECV_BARRIER(resource, trap, external)                                                               \
    edf_channel_test_hook_recv_barrier((resource), (const void *)(trap), (external))

#else

#define EDF_CHANNEL_TEST_HOOK_EVENT(name, resource, trap, external) ((void)0)
#define EDF_CHANNEL_TEST_HOOK_RECV_BARRIER(resource, trap, external) (0)

#endif

#ifdef __cplusplus
}
#endif

#endif
