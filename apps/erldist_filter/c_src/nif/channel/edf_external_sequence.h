/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * Copyright (c) WhatsApp LLC
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE.md file in the root directory of this source tree.
 */

#ifndef EDF_EXTERNAL_SEQUENCE_H
#define EDF_EXTERNAL_SEQUENCE_H

#ifdef __cplusplus
extern "C" {
#endif

#include "edf_external.h"

/* Macro Definitions */

/* Type Definitions */

/* Function Declarations */

static void edf_external_sequence_init_empty(edf_external_t *ext);
static bool edf_external_sequence_is_linked(edf_external_t *ext);
extern void edf_external_sequence_destroy_all(edf_external_t **root);
extern edf_external_t *edf_external_sequence_lookup(edf_external_t *root, uint64_t sequence_id);
extern edf_external_t *edf_external_sequence_lookup_insert(edf_external_t **root, edf_external_t *ext);
extern void edf_external_sequence_unlink(edf_external_t **root, edf_external_t *ext);

/* Inline Function Definitions */

inline void
edf_external_sequence_init_empty(edf_external_t *ext)
{
    ext->_sequence.parent = NULL;
    ext->_sequence.left = NULL;
    ext->_sequence.right = NULL;
    ext->_sequence.is_red = 0;
    ext->_sequence.is_linked = 0;
    return;
}

inline bool
edf_external_sequence_is_linked(edf_external_t *ext)
{
    if (ext == NULL || ext->_sequence.is_linked == 0) {
        return false;
    }
    return true;
}

#ifdef __cplusplus
}
#endif

#endif
