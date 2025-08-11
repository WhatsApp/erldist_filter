/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * Copyright (c) WhatsApp LLC
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE.md file in the root directory of this source tree.
 */

#ifndef SLICE_H
#define SLICE_H

#ifdef __cplusplus
extern "C" {
#endif

#include <inttypes.h>
#include <stdint.h>

/* Macro Definitions */

#define SLICE_INIT_EMPTY()                                                                                                         \
    {                                                                                                                              \
        .head = NULL,                                                                                                              \
        .tail = NULL,                                                                                                              \
    }

/* Type Definitions */

typedef struct slice_s slice_t;

struct slice_s {
    const uint8_t *head;
    const uint8_t *tail;
};

/* Function Declarations */

static int slice_is_empty(const slice_t *slice);
static size_t slice_len(const slice_t *slice);

/* Inline Function Definitions */

inline int
slice_is_empty(const slice_t *slice)
{
    return (slice == NULL || slice->head == NULL || slice->tail == NULL);
}

inline size_t
slice_len(const slice_t *slice)
{
    return (size_t)(slice->tail - slice->head);
}

#ifdef __cplusplus
}
#endif

#endif
