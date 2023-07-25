/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * Copyright (c) WhatsApp LLC
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE.md file in the root directory of this source tree.
 */

#ifndef CORE_SIMD_H
#define CORE_SIMD_H

#ifdef __cplusplus
extern "C" {
#endif

#include <emmintrin.h>
#include <immintrin.h>
#include <inttypes.h>
#include <stdint.h>

#include "xnif_trace.h"

// #define CORE_SIMD_DEBUG 1

#ifdef CORE_SIMD_DEBUG
#define CORE_SIMD_TRACE_F(...) XNIF_TRACE_F(__VA_ARGS__)
#else
#define CORE_SIMD_TRACE_F(...) ((void)(0))
#endif

/* Type Definitions */

/* Function Declarations */

#if defined(__AVX512F__)
static void core_simd_add_vec_u64_avx512f(uint64_t *a, const uint64_t *b, size_t size);
#endif
#if defined(__AVX2__)
static void core_simd_add_vec_u64_avx2(uint64_t *a, const uint64_t *b, size_t size);
#endif
#if defined(__SSE2__)
static void core_simd_add_vec_u64_sse2(uint64_t *a, const uint64_t *b, size_t size);
#endif
static void core_simd_add_vec_u64_scalar(uint64_t *a, const uint64_t *b, size_t size);
static void core_simd_add_vec_u64(uint64_t *a, const uint64_t *b, size_t size);

/* Inline Function Definitions */

#if defined(__AVX512F__)
inline void
core_simd_add_vec_u64_avx512f(uint64_t *a, const uint64_t *b, size_t size)
{
    size_t i;
    for (i = 0; (i + 8) <= size; i += 8) {
        __m512i vec1 = _mm512_loadu_si512((__m512i *)(a + i));
        __m512i vec2 = _mm512_loadu_si512((__m512i *)(b + i));
        __m512i result = _mm512_add_epi64(vec1, vec2);
        (void)_mm512_storeu_si512((__m512i *)(a + i), result);
    }
    return;
}
#endif

#if defined(__AVX2__)
inline void
core_simd_add_vec_u64_avx2(uint64_t *a, const uint64_t *b, size_t size)
{
    size_t i;
    for (i = 0; (i + 4) <= size; i += 4) {
        __m256i vec1 = _mm256_loadu_si256((__m256i_u *)(a + i));
        __m256i vec2 = _mm256_loadu_si256((__m256i_u *)(b + i));
        __m256i result = _mm256_add_epi64(vec1, vec2);
        (void)_mm256_storeu_si256((__m256i_u *)(a + i), result);
    }
    return;
}
#endif

#if defined(__SSE2__)
inline void
core_simd_add_vec_u64_sse2(uint64_t *a, const uint64_t *b, size_t size)
{
    size_t i;
    for (i = 0; (i + 2) <= size; i += 2) {
        __m128i vec1 = _mm_loadu_si128((__m128i_u *)(a + i));
        __m128i vec2 = _mm_loadu_si128((__m128i_u *)(b + i));
        __m128i result = _mm_add_epi64(vec1, vec2);
        (void)_mm_storeu_si128((__m128i_u *)(a + i), result);
    }
    return;
}
#endif

inline void
core_simd_add_vec_u64_scalar(uint64_t *a, const uint64_t *b, size_t size)
{
    size_t i;
    for (i = 0; i < size; i += 1) {
        a[i] += b[i];
    }
    return;
}

inline void
core_simd_add_vec_u64(uint64_t *a, const uint64_t *b, size_t size)
{
    size_t remainder = size;
#if defined(__AVX512F__)
    if (remainder >= 8 && __builtin_cpu_supports("avx512f")) {
        CORE_SIMD_TRACE_F("%s:%d core_simd_add_vec_u64_avx512f() size=%llu, remainder=%llu\n", __FILE__, __LINE__, size, remainder);
        (void)core_simd_add_vec_u64_avx512f(a, b, remainder);
        remainder %= 8;
    }
#endif
#if defined(__AVX2__)
    if (remainder >= 4 && __builtin_cpu_supports("avx2")) {
        CORE_SIMD_TRACE_F("%s:%d core_simd_add_vec_u64_avx2() size=%llu, remainder=%llu\n", __FILE__, __LINE__, size, remainder);
        (void)core_simd_add_vec_u64_avx2(a, b, remainder);
        remainder %= 4;
    }
#endif
#if defined(__SSE2__)
    if (remainder >= 2 && __builtin_cpu_supports("sse2")) {
        CORE_SIMD_TRACE_F("%s:%d core_simd_add_vec_u64_sse2() size=%llu, remainder=%llu\n", __FILE__, __LINE__, size, remainder);
        (void)core_simd_add_vec_u64_sse2(a, b, remainder);
        remainder %= 2;
    }
#endif
    if (remainder > 0) {
        CORE_SIMD_TRACE_F("%s:%d core_simd_add_vec_u64_scalar() size=%llu, remainder=%llu\n", __FILE__, __LINE__, size, remainder);
        (void)core_simd_add_vec_u64_scalar(a, b, remainder);
    }
    return;
}

#undef CORE_SIMD_TRACE_F

#ifdef __cplusplus
}
#endif

#endif
