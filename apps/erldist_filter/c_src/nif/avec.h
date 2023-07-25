/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * Copyright (c) WhatsApp LLC
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE.md file in the root directory of this source tree.
 */

#ifndef AVEC_H
#define AVEC_H

#ifdef __cplusplus
extern "C" {
#endif

#include "action.h"

/* Macros */

#define AVEC_FIXED_CAPACITY (4)

/* Type Definitions */

typedef struct avec_s avec_t;
typedef enum avec_tag_t avec_tag_t;
typedef struct avec_data_fixed_s avec_data_fixed_t;
typedef struct avec_data_owned_s avec_data_owned_t;

struct avec_data_fixed_s {
    size_t cap;
    size_t len;
    action_t actions[AVEC_FIXED_CAPACITY];
};

struct avec_data_owned_s {
    size_t cap;
    size_t len;
    action_t *actions;
};

enum avec_tag_t {
    AVEC_TAG_FREE = 0,
    AVEC_TAG_FIXED,
    AVEC_TAG_OWNED,
};

struct avec_s {
    avec_tag_t tag;
    union {
        avec_data_fixed_t fixed;
        avec_data_owned_t owned;
    } data;
};

/* Function Declarations */

static int avec_init_free(avec_t *avec);
static int avec_create_fixed(avec_t *avec);
extern int avec_create_owned(avec_t *avec, size_t capacity);
extern int avec_realloc_owned(avec_t *avec, size_t new_capacity);
extern void avec_destroy(avec_t *avec);

static size_t avec_capacity(const avec_t *avec);
extern int avec_into_list_term(ErlNifEnv *env, avec_t *avec, ERL_NIF_TERM *listp);
static int avec_is_free(const avec_t *avec);
static int avec_is_fixed(const avec_t *avec);
static int avec_is_owned(const avec_t *avec);
static size_t avec_len(const avec_t *avec);
extern int avec_pop(avec_t *avec, action_t *actionp);
extern int avec_push(avec_t *avec, action_t *action);

/* Inline Function Definitions */

inline int
avec_init_free(avec_t *avec)
{
    XNIF_TRACE_F("%s:%d avec_init_free()\n", __FILE__, __LINE__);
    avec->tag = AVEC_TAG_FREE;
    return 1;
}

inline int
avec_create_fixed(avec_t *avec)
{
    int i;
    if (!avec_is_free(avec)) {
        return 0;
    }
    avec->tag = AVEC_TAG_FIXED;
    avec->data.fixed.cap = AVEC_FIXED_CAPACITY;
    avec->data.fixed.len = 0;
    for (i = 0; i < AVEC_FIXED_CAPACITY; i++) {
        (void)action_init_free(&avec->data.fixed.actions[i]);
    }
    return 1;
}

inline size_t
avec_capacity(const avec_t *avec)
{
    switch (avec->tag) {
    case AVEC_TAG_FIXED:
        return avec->data.fixed.cap;
    case AVEC_TAG_OWNED:
        return avec->data.owned.cap;
    default:
        return 0;
    }
}

inline int
avec_is_free(const avec_t *avec)
{
    return (avec->tag == AVEC_TAG_FREE);
}

inline int
avec_is_fixed(const avec_t *avec)
{
    return (avec->tag == AVEC_TAG_FIXED);
}

inline int
avec_is_owned(const avec_t *avec)
{
    return (avec->tag == AVEC_TAG_OWNED);
}

inline size_t
avec_len(const avec_t *avec)
{
    switch (avec->tag) {
    case AVEC_TAG_FIXED:
        return avec->data.fixed.len;
    case AVEC_TAG_OWNED:
        return avec->data.owned.len;
    default:
        return 0;
    }
}

#ifdef __cplusplus
}
#endif

#endif
