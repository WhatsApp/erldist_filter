/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * Copyright (c) WhatsApp LLC
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE.md file in the root directory of this source tree.
 */

#ifndef IPC_H
#define IPC_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stdatomic.h>

#include "../../primitive/linklist.h"

#include "xnif_mutex.h"
#include "xnif_trace.h"

/* Type Definitions */

typedef struct ipc_batch_s ipc_batch_t;
typedef struct ipc_message_s ipc_message_t;
typedef struct ipc_queue_s ipc_queue_t;
typedef void (*ipc_queue_message_dtor_t)(ipc_message_t *message);

struct ipc_batch_s {
    linklist_t _link;
    ipc_queue_message_dtor_t message_dtor;
    size_t size;
    size_t drop;
};

struct ipc_message_s {
    linklist_t _link;
};

struct ipc_queue_s {
    linklist_t _link;
    xnif_mutex_t mutex;
    ipc_queue_message_dtor_t message_dtor;
    struct {
        atomic_flag flag;
        int readfd;
        int writefd;
    } async;
    size_t capacity;
    size_t size;
    size_t drop;
};

/* Function Declarations */

static void ipc_batch_init(ipc_batch_t *batch);
static void ipc_batch_dtor(ipc_batch_t *batch);
static void ipc_batch_move(ipc_batch_t *dst, ipc_batch_t *src);
static int ipc_message_create(size_t size, ipc_message_t **messagep);
static void ipc_message_destroy(ipc_message_t *message);
extern int ipc_queue_create(ipc_queue_t *queue, size_t capacity, char *mutex_name, ipc_queue_message_dtor_t message_dtor);
extern void ipc_queue_destroy(ipc_queue_t *queue);
extern int ipc_queue_clone_reader(ipc_queue_t *queue, int *readerp);
extern int ipc_queue_recv(ipc_queue_t *queue, ipc_batch_t *batch);
static int ipc_queue_send(ipc_queue_t *queue, ipc_message_t *message);
extern int ipc_queue_send_multi(ipc_queue_t *queue, int messagec, ipc_message_t *messagev[]);
extern int ipc_queue_set_capacity(ipc_queue_t *queue, size_t new_capacity, size_t *old_capacity);

/* Inline Function Definitions */

inline int
ipc_queue_send(ipc_queue_t *queue, ipc_message_t *message)
{
    return ipc_queue_send_multi(queue, 1, &message);
}

inline void
ipc_batch_init(ipc_batch_t *batch)
{
    (void)linklist_init_anchor(&batch->_link);
    batch->message_dtor = NULL;
    batch->size = 0;
    batch->drop = 0;
    return;
}

inline void
ipc_batch_dtor(ipc_batch_t *batch)
{
    assert(linklist_is_linked(&batch->_link));
    linklist_t *root = (void *)&batch->_link;
    linklist_t *node = root->next;
    linklist_t *temp = NULL;
    ipc_message_t *message = NULL;
    while (node != root) {
        temp = node->next;
        (void)linklist_unlink(node);
        message = (void *)node;
        if (batch->message_dtor == NULL) {
            (void)ipc_message_destroy(message);
        } else {
            (void)batch->message_dtor(message);
        }
        node = temp;
    }
    (void)ipc_batch_init(batch);
    return;
}

inline void
ipc_batch_move(ipc_batch_t *dst, ipc_batch_t *src)
{
    assert(linklist_is_empty(&dst->_link));
    (void)linklist_insert_list(&dst->_link, &src->_link);
    dst->message_dtor = src->message_dtor;
    dst->size = src->size;
    dst->drop = src->drop;
    (void)ipc_batch_init(src);
    return;
}

inline int
ipc_message_create(size_t size, ipc_message_t **messagep)
{
    ipc_message_t *message = NULL;
    if (size < sizeof(ipc_message_t)) {
        return 0;
    }
    message = (void *)enif_alloc(size);
    if (message == NULL) {
        return 0;
    }
    message->_link.next = NULL;
    message->_link.prev = NULL;
    *messagep = message;
    return 1;
}

inline void
ipc_message_destroy(ipc_message_t *message)
{
    (void)enif_free((void *)message);
    return;
}

#ifdef __cplusplus
}
#endif

#endif
