/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * Copyright (c) WhatsApp LLC
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE.md file in the root directory of this source tree.
 */

#ifndef _GNU_SOURCE
#define _GNU_SOURCE /* See feature_test_macros(7) */
#endif
#include <fcntl.h> /* O_CLOEXEC and O_NONBLOCK, if supported. */
#include <unistd.h>

#include "edf_logger.h"
#include "../erts/external.h"

/* Global Variables */

ErlNifResourceType *edf_logger_resource_type = NULL;
static edf_logger_resource_table_t edf_logger_resource_table_internal = {._link = {.next = NULL, .prev = NULL}};
edf_logger_resource_table_t *edf_logger_resource_table = &edf_logger_resource_table_internal;
static atomic_flag edf_logger_queue_global_flag = ATOMIC_FLAG_INIT;
static edf_logger_queue_t edf_logger_queue_global_internal;
edf_logger_queue_t *edf_logger_queue_global = &edf_logger_queue_global_internal;

/* Static Declarations */

static void edf_logger_resource_type_dtor(ErlNifEnv *env, void *obj);
static void edf_logger_resource_type_stop(ErlNifEnv *env, void *obj, ErlNifEvent event, int is_direct_call);
static void edf_logger_resource_type_down(ErlNifEnv *env, void *obj, ErlNifPid *pid, ErlNifMonitor *mon);

static int edf_logger_create(ErlNifEnv *env, edf_logger_resource_t *resource, edf_logger_t **loggerp, ERL_NIF_TERM *error_term);
static void edf_logger_queue_message_dtor(ipc_message_t *message);
static int edf_logger_queue_create(edf_logger_queue_t *queue, size_t capacity, char *mutex_name);
static void edf_logger_queue_destroy(edf_logger_queue_t *queue);

/* Function Definitions */

int
edf_logger_load(ErlNifEnv *env)
{
    int retval = 0;

    static ErlNifResourceTypeInit edf_logger_resource_type_init = {
        .dtor = edf_logger_resource_type_dtor,
        .stop = edf_logger_resource_type_stop,
        .down = edf_logger_resource_type_down,
        .members = 4,
        .dyncall = NULL,
    };
    edf_logger_resource_type = enif_init_resource_type(env, "erldist_filter_nif_logger", &edf_logger_resource_type_init,
                                                       ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER, NULL);
    if (edf_logger_resource_type == NULL) {
        retval = -1;
        return retval;
    }

    if (!linklist_is_linked(&edf_logger_resource_table->_link)) {
        (void)xnif_mutex_create(&edf_logger_resource_table->mutex, "erldist_filter.logger_resource_table_mutex");
        (void)linklist_init_anchor(&edf_logger_resource_table->_link);
    }

    if (atomic_flag_test_and_set_explicit(&edf_logger_queue_global_flag, memory_order_relaxed) == false) {
        if (!edf_logger_queue_create(edf_logger_queue_global, EDF_LOGGER_DEFAULT_CAPACITY,
                                     "erldist_filter.logger_queue_global_mutex")) {
            (void)atomic_flag_clear_explicit(&edf_logger_queue_global_flag, memory_order_relaxed);
            retval = -1;
            return retval;
        }
    }

    return retval;
}

void
edf_logger_unload(ErlNifEnv *env)
{
    (void)env;
    edf_logger_resource_type = NULL;
    (void)edf_logger_queue_destroy(edf_logger_queue_global);
    (void)atomic_flag_clear_explicit(&edf_logger_queue_global_flag, memory_order_relaxed);
    return;
}

ERL_NIF_TERM
edf_logger_resource_open(ErlNifEnv *env, edf_logger_resource_t **resourcep, edf_logger_t **loggerp)
{
    edf_logger_resource_t *resource = NULL;
    edf_logger_t *logger = NULL;
    ERL_NIF_TERM out_term;

    resource = enif_alloc_resource(edf_logger_resource_type, sizeof(edf_logger_resource_t));
    if (resource == NULL) {
        return EXCP_ERROR(env, "Can't allocate edf_logger_resource_t");
    }

    resource->_link.next = NULL;
    resource->_link.prev = NULL;
    if (!xnif_rwlock_create(&resource->rwlock, "erldist_filter.logger_resource_rwlock")) {
        (void)enif_release_resource((void *)resource);
        return EXCP_ERROR(env, "Call to xnif_rwlock_create() failed: Can't allocate xnif_rwlock_t");
    }
    resource->inner = NULL;
    resource->closefd = -1;

    if (!edf_logger_create(env, resource, &logger, &out_term)) {
        (void)enif_release_resource((void *)resource);
        return EXCP_ERROR(env, "Call to edf_logger_create() failed: Can't allocate edf_logger_t");
    }

    resource->inner = logger;

    out_term = enif_make_resource(env, (void *)resource);
    (void)xnif_mutex_lock(&edf_logger_resource_table->mutex);
    (void)linklist_insert(&edf_logger_resource_table->_link, &resource->_link);
    (void)xnif_mutex_unlock(&edf_logger_resource_table->mutex);
    // Don't release the resource here, so a reference is kept on the root table.
    // (void)enif_release_resource((void *)resource);

    if (resourcep != NULL) {
        *resourcep = resource;
    }
    if (loggerp != NULL) {
        *loggerp = logger;
    }

    return out_term;
}

void
edf_logger_resource_type_dtor(ErlNifEnv *env, void *obj)
{
    edf_logger_resource_t *resource = (void *)obj;
    edf_logger_t *logger = NULL;
    XNIF_TRACE_F("[logger] dtor callback\n");
    (void)xnif_rwlock_write_lock(&(resource->rwlock));
    logger = resource->inner;
    if (logger != NULL) {
        resource->inner = NULL;
        logger->resource = NULL;
        (void)edf_logger_destroy(env, resource, logger);
        logger = NULL;
    }
    (void)xnif_mutex_lock(&edf_logger_resource_table->mutex);
    if (linklist_is_linked(&resource->_link)) {
        (void)linklist_unlink(&resource->_link);
    }
    (void)xnif_mutex_unlock(&edf_logger_resource_table->mutex);
    (void)xnif_rwlock_write_unlock(&(resource->rwlock));
    (void)xnif_rwlock_destroy(&(resource->rwlock));
    return;
}

void
edf_logger_resource_type_stop(ErlNifEnv *env, void *obj, ErlNifEvent event, int is_direct_call)
{
    edf_logger_resource_t *resource = (void *)obj;
    edf_logger_t *logger = NULL;
    XNIF_TRACE_F("[logger] stop callback\n");
    if (!is_direct_call) {
        (void)xnif_rwlock_write_lock(&(resource->rwlock));
    }
    if (resource->closefd != -1) {
        (void)close(resource->closefd);
        resource->closefd = -1;
    }
    logger = resource->inner;
    if (logger == NULL) {
        if (!is_direct_call) {
            (void)xnif_rwlock_write_unlock(&(resource->rwlock));
        }
        return;
    }
    (void)edf_logger_destroy(env, resource, logger);
    if (!is_direct_call) {
        (void)xnif_rwlock_write_unlock(&(resource->rwlock));
    }
    return;
}

void
edf_logger_resource_type_down(ErlNifEnv *env, void *obj, ErlNifPid *pid, ErlNifMonitor *mon)
{
    edf_logger_resource_t *resource = (void *)obj;
    edf_logger_t *logger = NULL;
    XNIF_TRACE_F("[logger] down callback\n");
    (void)xnif_rwlock_write_lock(&(resource->rwlock));
    logger = resource->inner;
    if (logger == NULL) {
        (void)xnif_rwlock_write_unlock(&(resource->rwlock));
        return;
    }
    if (enif_compare_monitors(&logger->owner.monitor, mon) == 0) {
        // Owner is down, close logger.
        (void)xnif_monitor_set_undefined(&logger->owner);
        resource->inner = NULL;
        logger->resource = NULL;
        (void)edf_logger_destroy(env, resource, logger);
        (void)xnif_rwlock_write_unlock(&(resource->rwlock));
        return;
    } else {
        // Old monitor down event, ignore.
    }
    (void)xnif_rwlock_write_unlock(&(resource->rwlock));
    return;
}

int
edf_logger_create(ErlNifEnv *env, edf_logger_resource_t *resource, edf_logger_t **loggerp, ERL_NIF_TERM *error_term)
{
    edf_logger_t *logger = NULL;
    int retval;

    logger = (edf_logger_t *)enif_alloc(sizeof(edf_logger_t));
    if (logger == NULL) {
        return 0;
    }
    logger->resource = resource;
    (void)xnif_monitor_set_undefined(&logger->owner);
    logger->select.active = false;
    logger->select.fd = -1;
    logger->select.handle = THE_NON_VALUE;
    logger->stats.received = 0;
    logger->stats.dropped = 0;

    retval = xnif_monitor_self(env, (void *)resource, &logger->owner);
    if (retval < 0) {
        (void)edf_logger_destroy(env, (void *)resource, logger);
        *error_term = EXCP_ERROR(env, "Call to enif_monitor_process() failed: no `down' callback provided");
        return 0;
    } else if (retval > 0) {
        (void)edf_logger_destroy(env, (void *)resource, logger);
        *error_term = EXCP_ERROR(env, "Call to enif_monitor_process() failed: target process is no longer alive");
        return 0;
    }

    if (!ipc_queue_clone_reader(&edf_logger_queue_global->super, &logger->select.fd)) {
        (void)edf_logger_destroy(env, (void *)resource, logger);
        *error_term = EXCP_ERROR(env, "Call to ipc_queue_clone_reader() failed");
        return 0;
    }

    *loggerp = logger;
    return 1;
}

void
edf_logger_destroy(ErlNifEnv *env, edf_logger_resource_t *resource, edf_logger_t *logger)
{
    XNIF_TRACE_F("%s:%d edf_logger_destroy()\n", __FILE__, __LINE__);
    if (logger == NULL) {
        resource->inner = NULL;
        return;
    }
    logger->resource = NULL;
    (void)xnif_demonitor_process(env, (void *)resource, &logger->owner);
    if (logger->select.fd != -1) {
        if (logger->select.active == true) {
            resource->closefd = logger->select.fd;
            (void)enif_select(env, logger->select.fd, ERL_NIF_SELECT_STOP, (void *)resource, NULL, ATOM(undefined));
        } else {
            resource->closefd = -1;
            (void)close(logger->select.fd);
        }
        logger->select.fd = -1;
    }
    resource->inner = NULL;
    (void)enif_free((void *)logger);
    return;
}

int
edf_logger_event_create(ERL_NIF_TERM sysname, size_t number_of_atoms, bool is_external, size_t control_size, size_t payload_size,
                        edf_logger_event_t **eventp)
{
    edf_logger_event_t *event = NULL;
    size_t control_size_extra = 0;
    size_t payload_size_extra = 0;
    if (!ipc_message_create(sizeof(edf_logger_event_t), (void *)&event)) {
        return 0;
    }
    event->time = enif_monotonic_time(ERL_NIF_NSEC);
    event->sysname = sysname;
    event->number_of_atoms = number_of_atoms;
    event->atoms = NULL;
    (void)vec_init_free(&event->control.vec);
    (void)vec_init_free(&event->payload.vec);
    (void)vec_writer_destroy(&event->control.writer);
    (void)vec_writer_destroy(&event->payload.writer);
    if (event->number_of_atoms > 0) {
        event->atoms = (void *)enif_alloc(sizeof(ERL_NIF_TERM) * event->number_of_atoms);
        if (event->atoms == NULL) {
            (void)edf_logger_event_destroy(event);
            return 0;
        }
    }
    if (!is_external) {
        control_size_extra += 1;
        payload_size_extra += 1;
    }
    if (control_size > 0 && !vec_create_owned(&event->control.vec, control_size + control_size_extra)) {
        (void)edf_logger_event_destroy(event);
        return 0;
    }
    if (payload_size > 0 && !vec_create_owned(&event->payload.vec, payload_size + payload_size_extra)) {
        (void)edf_logger_event_destroy(event);
        return 0;
    }
    if (control_size > 0) {
        (void)vec_writer_create(&event->control.writer, &event->control.vec, 0);
        if (!is_external && !vec_writer_write_u8(&event->control.writer, VERSION_MAGIC)) {
            (void)edf_logger_event_destroy(event);
            return 0;
        }
    }
    if (payload_size > 0) {
        (void)vec_writer_create(&event->payload.writer, &event->payload.vec, 0);
        if (!is_external && !vec_writer_write_u8(&event->payload.writer, VERSION_MAGIC)) {
            (void)edf_logger_event_destroy(event);
            return 0;
        }
    }
    *eventp = event;
    return 1;
}

void
edf_logger_event_destroy(edf_logger_event_t *event)
{
    (void)vec_writer_destroy(&event->payload.writer);
    (void)vec_writer_destroy(&event->control.writer);
    (void)vec_destroy(&event->payload.vec);
    (void)vec_destroy(&event->control.vec);
    if (event->atoms != NULL) {
        (void)enif_free((void *)event->atoms);
        event->atoms = NULL;
    }
    event->number_of_atoms = 0;
    event->sysname = THE_NON_VALUE;
    event->time = (ErlNifTime)0;
    (void)ipc_message_destroy(&event->super);
    return;
}

int
edf_logger_event_into_term(ErlNifEnv *env, edf_logger_event_t *event, ERL_NIF_TERM *termp)
{
    ERL_NIF_TERM element = THE_NON_VALUE;
    ERL_NIF_TERM atoms_term = THE_NON_VALUE;
    ERL_NIF_TERM control_term = THE_NON_VALUE;
    ERL_NIF_TERM payload_term = THE_NON_VALUE;
    if (event == NULL) {
        return 0;
    }
    atoms_term = (event->number_of_atoms > 0) ? enif_make_tuple_from_array(env, event->atoms, event->number_of_atoms)
                                              : enif_make_tuple(env, 0);
    control_term = (vec_len(&event->control.vec) > 0) ? vec_into_binary_term(env, &event->control.vec) : ATOM(undefined);
    payload_term = (vec_len(&event->payload.vec) > 0) ? vec_into_binary_term(env, &event->payload.vec) : ATOM(undefined);
    element = enif_make_tuple2(env, enif_make_int64(env, event->time),
                               enif_make_tuple4(env, event->sysname, atoms_term, control_term, payload_term));
    *termp = element;
    return 1;
}

void
edf_logger_queue_message_dtor(ipc_message_t *message)
{
    (void)edf_logger_event_destroy((void *)message);
    return;
}

int
edf_logger_queue_create(edf_logger_queue_t *queue, size_t capacity, char *mutex_name)
{
    if (!ipc_queue_create(&queue->super, capacity, mutex_name, edf_logger_queue_message_dtor)) {
        return 0;
    }
    return 1;
}

void
edf_logger_queue_destroy(edf_logger_queue_t *queue)
{
    ipc_batch_t batch[1];
    linklist_t *root = NULL;
    linklist_t *node = NULL;
    linklist_t *temp = NULL;
    edf_logger_event_t *event = NULL;
    if (queue == NULL) {
        return;
    }
    (void)ipc_batch_init(batch);
    if (ipc_queue_recv(&queue->super, batch)) {
        root = &batch->_link;
        node = root->prev;
        temp = NULL;
        event = NULL;
        while (node != root) {
            temp = node->prev;
            (void)linklist_unlink(node);
            event = (void *)node;
            (void)edf_logger_event_destroy(event);
            node = temp;
        }
    }
    (void)ipc_batch_dtor(batch);
    (void)xnif_mutex_lock(&queue->super.mutex);
    (void)ipc_queue_destroy(&queue->super);
    return;
}
