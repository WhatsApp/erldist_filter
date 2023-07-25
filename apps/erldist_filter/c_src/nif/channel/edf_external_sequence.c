/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * Copyright (c) WhatsApp LLC
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE.md file in the root directory of this source tree.
 */

#include "edf_external_sequence.h"

#ifndef Sint
typedef signed int Sint;
#endif

#ifndef Uint
typedef unsigned int Uint;
#endif

#ifndef ERTS_INLINE
#define ERTS_INLINE inline
#endif

#define ERTS_RBT_PREFIX edf_external_sequence
#define ERTS_RBT_T edf_external_t
#define ERTS_RBT_KEY_T uint64_t
#define ERTS_RBT_FLAGS_T int
#define ERTS_RBT_INIT_EMPTY_TNODE(T)                                                                                               \
    do {                                                                                                                           \
        (void)edf_external_sequence_init_empty((T));                                                                               \
    } while (0)
#define ERTS_RBT_IS_RED(T) ((T)->_sequence.is_red)
#define ERTS_RBT_SET_RED(T) ((T)->_sequence.is_red = 1)
#define ERTS_RBT_IS_BLACK(T) (!ERTS_RBT_IS_RED(T))
#define ERTS_RBT_SET_BLACK(T) ((T)->_sequence.is_red = 0)
#define ERTS_RBT_GET_FLAGS(T) ((T)->_sequence.is_red)
#define ERTS_RBT_SET_FLAGS(T, F) ((T)->_sequence.is_red = F)
#define ERTS_RBT_GET_PARENT(T) ((T)->_sequence.parent)
#define ERTS_RBT_SET_PARENT(T, P) ((T)->_sequence.parent = P)
#define ERTS_RBT_GET_RIGHT(T) ((T)->_sequence.right)
#define ERTS_RBT_SET_RIGHT(T, R) ((T)->_sequence.right = (R))
#define ERTS_RBT_GET_LEFT(T) ((T)->_sequence.left)
#define ERTS_RBT_SET_LEFT(T, L) ((T)->_sequence.left = (L))
#define ERTS_RBT_GET_KEY(T) ((T)->sequence_id)
#define ERTS_RBT_IS_LT(KX, KY) (KX < KY)
#define ERTS_RBT_IS_EQ(KX, KY) (KX == KY)
#define ERTS_RBT_WANT_DELETE
#define ERTS_RBT_WANT_LOOKUP
#define ERTS_RBT_WANT_LOOKUP_INSERT
#define ERTS_RBT_WANT_FOREACH
#define ERTS_RBT_WANT_FOREACH_DESTROY
#define ERTS_RBT_UNDEF

#include "../erts/erl_rbtree.h"

static int edf_external_sequence_foreach_destroy_callback(edf_external_t *ext, void *arg, signed int reds);

int
edf_external_sequence_foreach_destroy_callback(edf_external_t *ext, void *arg, signed int reds)
{
    (void)arg;
    (void)reds;
    ext->_sequence.is_linked = 0;
    (void)edf_external_destroy(ext);
    return 1;
}

void
edf_external_sequence_destroy_all(edf_external_t **root)
{
    if (root == NULL || *root == NULL) {
        return;
    }
    (void)edf_external_sequence_rbt_foreach_destroy(root, edf_external_sequence_foreach_destroy_callback, NULL);
    return;
}

edf_external_t *
edf_external_sequence_lookup(edf_external_t *root, uint64_t sequence_id)
{
    return edf_external_sequence_rbt_lookup(root, sequence_id);
}

edf_external_t *
edf_external_sequence_lookup_insert(edf_external_t **root, edf_external_t *ext)
{
    edf_external_t *new_ext = edf_external_sequence_rbt_lookup_insert(root, ext);
    if (new_ext == NULL) {
        ext->_sequence.is_linked = 1;
    }
    return new_ext;
}

void
edf_external_sequence_unlink(edf_external_t **root, edf_external_t *ext)
{
    if (root == NULL || *root == NULL || !edf_external_sequence_is_linked(ext)) {
        return;
    }
    (void)edf_external_sequence_rbt_delete(root, ext);
    ext->_sequence.is_linked = 0;
    return;
}
