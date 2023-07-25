/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * Copyright (c) WhatsApp LLC
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE.md file in the root directory of this source tree.
 */

#include "avec.h"

static void avec_data_fixed_destroy(avec_data_fixed_t *data);
static void avec_data_owned_destroy(avec_data_owned_t *data);
static int avec_pop_fixed(avec_t *avec, action_t *actionp);
static int avec_pop_owned(avec_t *avec, action_t *actionp);
static int avec_push_fixed(avec_t *avec, action_t *action);
static int avec_push_owned(avec_t *avec, action_t *action);

void
avec_destroy(avec_t *avec)
{
    XNIF_TRACE_F("%s:%d avec_destroy()\n", __FILE__, __LINE__);
    switch (avec->tag) {
    case AVEC_TAG_FIXED:
        avec->tag = AVEC_TAG_FREE;
        (void)avec_data_fixed_destroy(&avec->data.fixed);
        break;
    case AVEC_TAG_OWNED:
        avec->tag = AVEC_TAG_FREE;
        (void)avec_data_owned_destroy(&avec->data.owned);
        break;
    default:
        break;
    }
}

inline void
avec_data_fixed_destroy(avec_data_fixed_t *data)
{
    XNIF_TRACE_F("%s:%d avec_data_fixed_destroy()\n", __FILE__, __LINE__);
    size_t i;
    for (i = 0; i < data->len; i++) {
        (void)action_destroy(&data->actions[i]);
    }
    data->cap = 0;
    data->len = 0;
    return;
}

inline void
avec_data_owned_destroy(avec_data_owned_t *data)
{
    XNIF_TRACE_F("%s:%d avec_data_owned_destroy()\n", __FILE__, __LINE__);
    size_t i;
    for (i = 0; i < data->len; i++) {
        (void)action_destroy(&data->actions[i]);
    }
    data->cap = 0;
    data->len = 0;
    (void)enif_free((void *)data->actions);
    data->actions = NULL;
    return;
}

int
avec_create_owned(avec_t *avec, size_t capacity)
{
    avec_data_owned_t *owned = &avec->data.owned;
    size_t i;

    XNIF_TRACE_F("%s:%d avec_create_owned(capacity=%u)\n", __FILE__, __LINE__, capacity);

    if (!avec_is_free(avec)) {
        XNIF_TRACE_F("Call to avec_create_owned() failed: avec is not free (%d)\n", avec->tag);
        return 0;
    }
    owned->actions = enif_alloc(sizeof(action_t) * capacity);
    if (owned->actions == NULL) {
        XNIF_TRACE_F("%s:%d Call to enif_alloc() failed\n", __FILE__, __LINE__);
        return 0;
    }
    owned->cap = capacity;
    owned->len = 0;
    for (i = 0; i < capacity; i++) {
        (void)action_init_free(&owned->actions[i]);
    }
    avec->tag = AVEC_TAG_OWNED;
    return 1;
}

int
avec_realloc_owned(avec_t *avec, size_t new_capacity)
{
    XNIF_TRACE_F("%s:%d avec_realloc_owned(new_capacity=%u)\n", __FILE__, __LINE__, new_capacity);
    avec_data_owned_t *owned = &avec->data.owned;
    // avec_t temp;
    action_t *new_actions = NULL;
    size_t i;
    // size_t new_len;

    if (!avec_is_owned(avec)) {
        return 0;
    }

    new_actions = enif_realloc(owned->actions, sizeof(action_t) * new_capacity);
    if (new_actions == NULL) {
        return 0;
    }
    owned->actions = new_actions;
    if (owned->cap < new_capacity) {
        for (i = owned->cap; i < new_capacity; i++) {
            (void)action_init_free(&owned->actions[i]);
        }
    }
    owned->cap = new_capacity;

    if (new_capacity < owned->len) {
        owned->len = new_capacity;
    }

    return 1;
}

int
avec_into_list_term(ErlNifEnv *env, avec_t *avec, ERL_NIF_TERM *listp)
{
    XNIF_TRACE_F("%s:%d avec_into_list_term()\n", __FILE__, __LINE__);
    ERL_NIF_TERM list;
    ERL_NIF_TERM term;
    action_t *actions = NULL;
    action_t *action = NULL;
    size_t len;
    size_t i;

    if (avec_is_fixed(avec)) {
        actions = avec->data.fixed.actions;
        len = avec->data.fixed.len;
    } else if (avec_is_owned(avec)) {
        actions = avec->data.owned.actions;
        len = avec->data.owned.len;
    } else {
        return 0;
    }

    list = enif_make_list(env, 0);
    for (i = len; i > 0; i -= 1) {
        action = &actions[i - 1];
        if (!action_into_term(env, action, &term)) {
            return 0;
        }
        list = enif_make_list_cell(env, term, list);
    }

    (void)avec_destroy(avec);
    *listp = list;
    return 1;
}

int
avec_pop(avec_t *avec, action_t *actionp)
{
    if (avec_is_fixed(avec)) {
        return avec_pop_fixed(avec, actionp);
    } else if (avec_is_owned(avec)) {
        return avec_pop_owned(avec, actionp);
    } else {
        return 0;
    }
}

inline int
avec_pop_fixed(avec_t *avec, action_t *actionp)
{
    avec_data_fixed_t *fixed = &avec->data.fixed;

    if (fixed->len > 0) {
        fixed->len -= 1;
        if (actionp != NULL) {
            *actionp = fixed->actions[fixed->len];
        }
        (void)action_init_free(&fixed->actions[fixed->len]);
        return 1;
    }
    return 0;
}

inline int
avec_pop_owned(avec_t *avec, action_t *actionp)
{
    avec_data_owned_t *owned = &avec->data.owned;

    if (owned->len > 0) {
        owned->len -= 1;
        if (actionp != NULL) {
            *actionp = owned->actions[owned->len];
        }
        (void)action_init_free(&owned->actions[owned->len]);
        return 1;
    }
    return 0;
}

int
avec_push(avec_t *avec, action_t *action)
{
    XNIF_TRACE_F("%s:%d avec_push()\n", __FILE__, __LINE__);
    if (avec_is_fixed(avec)) {
        return avec_push_fixed(avec, action);
    } else if (avec_is_owned(avec)) {
        return avec_push_owned(avec, action);
    } else {
        return 0;
    }
}

inline int
avec_push_fixed(avec_t *avec, action_t *action)
{
    XNIF_TRACE_F("%s:%d avec_push_fixed()\n", __FILE__, __LINE__);
    avec_data_fixed_t *fixed = &avec->data.fixed;
    avec_t temp;
    size_t i;

    if (fixed->len < fixed->cap) {
        fixed->actions[fixed->len] = *action;
        fixed->len += 1;
        (void)action_init_free(action);
        return 1;
    }
    (void)avec_init_free(&temp);
    if (!avec_create_owned(&temp, fixed->cap * 2)) {
        return 0;
    }
    for (i = 0; i < fixed->cap; i++) {
        temp.data.owned.actions[i] = fixed->actions[i];
        temp.data.owned.len += 1;
    }
    avec->tag = AVEC_TAG_OWNED;
    avec->data.owned = temp.data.owned;
    return avec_push_owned(avec, action);
}

inline int
avec_push_owned(avec_t *avec, action_t *action)
{
    XNIF_TRACE_F("%s:%d avec_push_owned()\n", __FILE__, __LINE__);
    avec_data_owned_t *owned = &avec->data.owned;

    if (owned->len < owned->cap) {
        owned->actions[owned->len] = *action;
        owned->len += 1;
        (void)action_init_free(action);
        return 1;
    }
    if (!avec_realloc_owned(avec, owned->cap * 2)) {
        return 0;
    }
    owned->actions[owned->len] = *action;
    owned->len += 1;
    (void)action_init_free(action);
    return 1;
}
