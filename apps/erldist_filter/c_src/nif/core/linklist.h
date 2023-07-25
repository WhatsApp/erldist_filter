/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * Copyright (c) WhatsApp LLC
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE.md file in the root directory of this source tree.
 */

#ifndef LINKLIST_H
#define LINKLIST_H

#ifdef __cplusplus
extern "C" {
#endif

#include <assert.h>
#include <stddef.h>

/* Type Definitions */

typedef struct linklist_s linklist_t;

struct linklist_s {
    linklist_t *next;
    linklist_t *prev;
};

/* Function Declarations */

static void linklist_init_anchor(linklist_t *anchor);
static int linklist_is_empty(linklist_t *anchor);
static int linklist_is_linked(linklist_t *node);
static void linklist_insert(linklist_t *pos, linklist_t *node);
static void linklist_insert_list(linklist_t *pos, linklist_t *list);
static void linklist_unlink(linklist_t *node);

/* Inline Function Definitions */

inline void
linklist_init_anchor(linklist_t *anchor)
{
    anchor->next = anchor->prev = anchor;
}

inline int
linklist_is_linked(linklist_t *node)
{
    return node->next != NULL;
}

inline int
linklist_is_empty(linklist_t *anchor)
{
    return anchor->next == anchor;
}

inline void
linklist_insert(linklist_t *pos, linklist_t *node)
{
    assert(!linklist_is_linked(node));

    node->prev = pos->prev;
    node->next = pos;
    node->prev->next = node;
    node->next->prev = node;
}

inline void
linklist_insert_list(linklist_t *pos, linklist_t *list)
{
    if (linklist_is_empty(list))
        return;
    list->next->prev = pos->prev;
    list->prev->next = pos;
    pos->prev->next = list->next;
    pos->prev = list->prev;
    (void)linklist_init_anchor(list);
}

inline void
linklist_unlink(linklist_t *node)
{
    node->next->prev = node->prev;
    node->prev->next = node->next;
    node->next = node->prev = NULL;
}

#ifdef __cplusplus
}
#endif

#endif
