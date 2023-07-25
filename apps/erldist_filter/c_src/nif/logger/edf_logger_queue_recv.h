/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * Copyright (c) WhatsApp LLC
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE.md file in the root directory of this source tree.
 */

#ifndef EDF_LOGGER_QUEUE_RECV_H
#define EDF_LOGGER_QUEUE_RECV_H

#ifdef __cplusplus
extern "C" {
#endif

#include "edf_logger.h"
#include "../trap/edf_trap.h"

/* Macro Definitions */

/* Type Definitions */

typedef struct edf_logger_queue_recv_trap_s edf_logger_queue_recv_trap_t;

struct edf_logger_queue_recv_trap_s {
    edf_trap_t super;
    edf_logger_resource_t *resource;
    edf_logger_t *logger;
    ipc_batch_t *batch;
};

/* Global Declarations */

/* Function Declarations */

extern int edf_logger_queue_recv(ErlNifEnv *env, edf_logger_resource_t *resource, edf_logger_t *logger, ipc_batch_t *batch,
                                 ERL_NIF_TERM *out_term);
extern ERL_NIF_TERM edf_logger_queue_recv_trap_open(ErlNifEnv *env, edf_logger_resource_t *resource, edf_logger_t *logger,
                                                    ipc_batch_t *batch, edf_logger_queue_recv_trap_t **trapp);

/* Inline Function Definitions */

#ifdef __cplusplus
}
#endif

#endif
