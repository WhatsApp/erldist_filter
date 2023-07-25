/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * Copyright (c) WhatsApp LLC
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE.md file in the root directory of this source tree.
 */

#define ERLDIST_FILTER_NIF_INTERNAL_API 1
#include "edf_logger_queue_recv.h"

#include "../erts/dist.h"
#include "../erts/external.h"

static int edf_logger_queue_recv_trap_acquire(ErlNifEnv *caller_env, edf_trap_t *super, void *arg, ERL_NIF_TERM *error_term);
static void edf_logger_queue_recv_trap_release(ErlNifEnv *caller_env, edf_trap_t *super, void *arg);
static void edf_logger_queue_recv_trap_dtor(ErlNifEnv *caller_env, edf_trap_t *super, void *arg);
static void edf_logger_queue_recv_trap_edit(ErlNifEnv *caller_env, edf_trap_t *super, void *arg, edf_trap_result_t *result);
static edf_trap_result_t edf_logger_queue_recv_trap_next(ErlNifEnv *caller_env, edf_trap_t *super, void *arg);

int
edf_logger_queue_recv(ErlNifEnv *env, edf_logger_resource_t *resource, edf_logger_t *logger, ipc_batch_t *batch,
                      ERL_NIF_TERM *out_term)
{
    if (!ipc_queue_recv(&edf_logger_queue_global->super, batch)) {
        *out_term = EXCP_BADARG(env, "Call to ipc_queue_recv() failed");
        return 0;
    }

    if (batch->drop == 0 && batch->size == 0) {
        ERL_NIF_TERM select_msg;
        int retval;

        logger->select.handle = enif_make_ref(env);
        select_msg = enif_make_tuple4(env, enif_make_atom(env, "$logger"), enif_make_resource(env, (void *)resource),
                                      enif_make_atom(env, "select"), logger->select.handle);
        retval = enif_select_read(env, logger->select.fd, (void *)resource, &logger->owner.pid, select_msg, NULL);
        if (retval < 0) {
            *out_term = EXCP_ERROR_F(env, "Call to enif_select_read() failed: retval=%d", retval);
            return 0;
        }
        logger->select.active = true;
        *out_term = enif_make_tuple2(env, enif_make_atom(env, "select"), logger->select.handle);
        return 0;
    }

    return 1;
}

ERL_NIF_TERM
edf_logger_queue_recv_trap_open(ErlNifEnv *env, edf_logger_resource_t *resource, edf_logger_t *logger, ipc_batch_t *batch,
                                edf_logger_queue_recv_trap_t **trapp)
{
    edf_trap_state_t trap_state = {
        .resource = (void *)resource,
        .acquire = edf_logger_queue_recv_trap_acquire,
        .release = edf_logger_queue_recv_trap_release,
        .dtor = edf_logger_queue_recv_trap_dtor,
        .edit = edf_logger_queue_recv_trap_edit,
        .next = edf_logger_queue_recv_trap_next,
        .arg = NULL,
    };
    edf_logger_queue_recv_trap_t *trap = NULL;
    ERL_NIF_TERM trap_term;

    trap_term = edf_trap_open(env, &trap_state, sizeof(edf_logger_queue_recv_trap_t) + sizeof(ipc_batch_t), (edf_trap_t **)(&trap));
    if (trap == NULL) {
        return trap_term;
    }

    trap->super.state.arg = (void *)trap;
    trap->resource = resource;
    trap->logger = logger;
    trap->batch = ((void *)trap) + sizeof(edf_logger_queue_recv_trap_t);
    (void)ipc_batch_init(trap->batch);
    (void)ipc_batch_move(trap->batch, batch);

    if (trapp != NULL) {
        *trapp = trap;
    }

    return trap_term;
}

int
edf_logger_queue_recv_trap_acquire(ErlNifEnv *caller_env, edf_trap_t *super, void *arg, ERL_NIF_TERM *error_term)
{
    edf_logger_queue_recv_trap_t *trap = (void *)arg;
    edf_logger_resource_t *resource = NULL;
    edf_logger_t *logger = NULL;
    int flags = (EDF_LOGGER_RESOURCE_FLAG_OWNER_REQUIRED | EDF_LOGGER_RESOURCE_FLAG_WRITE_LOCK);

    (void)super;

    resource = trap->resource;

    // Silence warnings about this function not being used.
    (void)edf_logger_resource_acquire;

    if (!edf_logger_resource_acquire_direct(caller_env, resource, &logger, error_term, flags)) {
        return 0;
    }

    if (trap->logger != logger) {
        *error_term = EXCP_ERROR_F(caller_env, "Fatal error: trap->logger=%p does not match logger=%p\n", trap->logger, logger);
        (void)edf_logger_resource_release(&resource, &logger, flags);
        return 0;
    }

    return 1;
}

void
edf_logger_queue_recv_trap_release(ErlNifEnv *caller_env, edf_trap_t *super, void *arg)
{
    edf_logger_queue_recv_trap_t *trap = (void *)arg;
    edf_logger_resource_t *resource = NULL;
    edf_logger_t *logger = NULL;
    int flags = (EDF_LOGGER_RESOURCE_FLAG_OWNER_REQUIRED | EDF_LOGGER_RESOURCE_FLAG_WRITE_LOCK);

    (void)caller_env;
    (void)super;

    resource = trap->resource;
    logger = trap->logger;

    (void)edf_logger_resource_release(&resource, &logger, flags);

    return;
}

void
edf_logger_queue_recv_trap_dtor(ErlNifEnv *caller_env, edf_trap_t *super, void *arg)
{
    edf_logger_queue_recv_trap_t *trap = (void *)arg;

    XNIF_TRACE_F("%s:%d [edf_logger_queue_recv_trap] dtor callback\n", __FILE__, __LINE__);

    (void)caller_env;
    (void)super;

    trap->resource = NULL;
    trap->logger = NULL;
    (void)ipc_batch_dtor(trap->batch);

    return;
}

void
edf_logger_queue_recv_trap_edit(ErlNifEnv *caller_env, edf_trap_t *super, void *arg, edf_trap_result_t *result)
{
    edf_logger_queue_recv_trap_t *trap = (void *)arg;
    edf_logger_resource_t *resource = trap->resource;
    edf_logger_t *logger = NULL;
    int flags = (EDF_LOGGER_RESOURCE_FLAG_WRITE_LOCK);

    if (result->tag == EDF_TRAP_RESULT_TAG_ERR) {
        // Any exception raised should close the logger, even if the exception is caught by the Erlang process.
        XNIF_TRACE_F("%s:%d [edf_logger_queue_recv_trap] error callback\n", __FILE__, __LINE__);

        if (resource != NULL && edf_logger_resource_acquire_direct(caller_env, resource, &logger, NULL, flags)) {
            (void)edf_logger_destroy(caller_env, resource, logger);
            trap->logger = NULL;
            (void)edf_logger_resource_release(&resource, &logger, flags);
        }
    }

    return;
}

edf_trap_result_t
edf_logger_queue_recv_trap_next(ErlNifEnv *caller_env, edf_trap_t *super, void *arg)
{
    edf_logger_queue_recv_trap_t *trap = (void *)arg;
    ERL_NIF_TERM *list = &trap->super.keep_term;
    linklist_t *root = &trap->batch->_link;
    linklist_t *node = root->prev;
    linklist_t *temp = NULL;
    edf_logger_event_t *event = NULL;
    ERL_NIF_TERM element = THE_NON_VALUE;
    ERL_NIF_TERM out_term;

    if (!enif_is_list(caller_env, *list)) {
        return TRAP_ERR(EXCP_BADARG(caller_env, "Corrupted trap->super.keep_term: must be a list\n"));
    }

    while (node != root) {
        temp = node->prev;
        (void)linklist_unlink(node);
        event = (void *)node;
        if (!edf_logger_event_into_term(caller_env, event, &element)) {
            return TRAP_ERR(EXCP_BADARG(caller_env, "Call to edf_logger_event_into_term() failed\n"));
        }
        *list = enif_make_list_cell(caller_env, element, *list);
        (void)edf_logger_event_destroy(event);
        node = temp;
        TRAP_REDUCE(trap, 1);
        if (TRAP_SHOULD_YIELD(trap)) {
            return TRAP_YIELD();
        }
    }

    trap->logger->stats.dropped += trap->batch->drop;
    trap->logger->stats.received += trap->batch->size;

    out_term = enif_make_tuple3(caller_env, enif_make_uint64(caller_env, (ErlNifUInt64)trap->batch->size),
                                enif_make_uint64(caller_env, (ErlNifUInt64)trap->batch->drop), *list);

    return TRAP_OK(out_term);
}
