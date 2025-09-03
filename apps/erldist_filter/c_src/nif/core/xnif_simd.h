/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * Copyright (c) WhatsApp LLC
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE.md file in the root directory of this source tree.
 */

#ifndef XNIF_SIMD_H
#define XNIF_SIMD_H

#ifdef __cplusplus
extern "C" {
#endif

#include "../../primitive/portable_hint.h"

#if defined(ARCH_ARM_AVAILABLE)
#include <arm_neon.h>
#elif defined(ARCH_X86_AVAILABLE)
#include <emmintrin.h>
#include <immintrin.h>
#include <inttypes.h>
#include <stdint.h>
#endif

#if defined(ARCH_ARM_AVAILABLE)
#if defined(__ARM_NEON) || defined(__aarch64__) || defined(_M_ARM64)
#define XNIF_SIMD_NEON 1
#endif
#elif defined(ARCH_X86_AVAILABLE)
#if defined(__AVX512F__)
#define XNIF_SIMD_AVX512F 1
#endif
#if defined(__AVX2__)
#define XNIF_SIMD_AVX2 1
#endif
#if defined(__SSE2__)
#define XNIF_SIMD_SSE2 1
#endif
#endif

#include "xnif_trace.h"

// #define XNIF_SIMD_DEBUG 1

#ifdef XNIF_SIMD_DEBUG
#define XNIF_SIMD_TRACE_F(...) XNIF_TRACE_F(__VA_ARGS__)
#else
#define XNIF_SIMD_TRACE_F(...) ((void)(0))
#endif

/* Type Definitions */

/* Function Declarations */

#if defined(XNIF_SIMD_AVX512F)
static void xnif_simd_add_vec_u64_avx512f(uint64_t *a, const uint64_t *b, size_t size);
#endif
#if defined(XNIF_SIMD_AVX2)
static void xnif_simd_add_vec_u64_avx2(uint64_t *a, const uint64_t *b, size_t size);
#endif
#if defined(XNIF_SIMD_NEON)
static void xnif_simd_add_vec_u64_neon(uint64_t *a, const uint64_t *b, size_t size);
#endif
#if defined(XNIF_SIMD_SSE2)
static void xnif_simd_add_vec_u64_sse2(uint64_t *a, const uint64_t *b, size_t size);
#endif
static void xnif_simd_add_vec_u64_scalar(uint64_t *a, const uint64_t *b, size_t size);
static void xnif_simd_add_vec_u64(uint64_t *a, const uint64_t *b, size_t size);

/* Inline Function Definitions */

#if defined(XNIF_SIMD_AVX512F)
inline void
xnif_simd_add_vec_u64_avx512f(uint64_t *a, const uint64_t *b, size_t size)
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

#if defined(XNIF_SIMD_AVX2)
inline void
xnif_simd_add_vec_u64_avx2(uint64_t *a, const uint64_t *b, size_t size)
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

#if defined(XNIF_SIMD_NEON)
inline void
xnif_simd_add_vec_u64_neon(uint64_t *a, const uint64_t *b, size_t size)
{
    size_t i;
    for (i = 0; (i + 2) <= size; i += 2) {
        uint64x2_t vec1 = vld1q_u64(a + i);
        uint64x2_t vec2 = vld1q_u64(b + i);
        uint64x2_t result = vaddq_u64(vec1, vec2);
        (void)vst1q_u64(a + i, result);
    }
    return;
}
#endif

#if defined(XNIF_SIMD_SSE2)
inline void
xnif_simd_add_vec_u64_sse2(uint64_t *a, const uint64_t *b, size_t size)
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
xnif_simd_add_vec_u64_scalar(uint64_t *a, const uint64_t *b, size_t size)
{
    size_t i;
    for (i = 0; i < size; i += 1) {
        a[i] += b[i];
    }
    return;
}

inline void
xnif_simd_add_vec_u64(uint64_t *a, const uint64_t *b, size_t size)
{
    size_t remainder = size;
#if defined(XNIF_SIMD_AVX512F)
    if (remainder >= 8 && __builtin_cpu_supports("avx512f")) {
        XNIF_SIMD_TRACE_F("%s:%d xnif_simd_add_vec_u64_avx512f() size=%llu, remainder=%llu\n", __FILE__, __LINE__, size, remainder);
        (void)xnif_simd_add_vec_u64_avx512f(&a[size - remainder], &b[size - remainder], remainder);
        remainder %= 8;
    }
#endif
#if defined(XNIF_SIMD_AVX2)
    if (remainder >= 4 && __builtin_cpu_supports("avx2")) {
        XNIF_SIMD_TRACE_F("%s:%d xnif_simd_add_vec_u64_avx2() size=%llu, remainder=%llu\n", __FILE__, __LINE__, size, remainder);
        (void)xnif_simd_add_vec_u64_avx2(&a[size - remainder], &b[size - remainder], remainder);
        remainder %= 4;
    }
#endif
#if defined(XNIF_SIMD_NEON)
    if (remainder >= 2) {
        XNIF_SIMD_TRACE_F("%s:%d xnif_simd_add_vec_u64_neon() size=%llu, remainder=%llu\n", __FILE__, __LINE__, size, remainder);
        (void)xnif_simd_add_vec_u64_neon(&a[size - remainder], &b[size - remainder], remainder);
        remainder %= 2;
    }
#endif
#if defined(XNIF_SIMD_SSE2)
    if (remainder >= 2 && __builtin_cpu_supports("sse2")) {
        XNIF_SIMD_TRACE_F("%s:%d xnif_simd_add_vec_u64_sse2() size=%llu, remainder=%llu\n", __FILE__, __LINE__, size, remainder);
        (void)xnif_simd_add_vec_u64_sse2(&a[size - remainder], &b[size - remainder], remainder);
        remainder %= 2;
    }
#endif
    if (remainder > 0) {
        XNIF_SIMD_TRACE_F("%s:%d xnif_simd_add_vec_u64_scalar() size=%llu, remainder=%llu\n", __FILE__, __LINE__, size, remainder);
        (void)xnif_simd_add_vec_u64_scalar(&a[size - remainder], &b[size - remainder], remainder);
    }
    return;
}

#undef XNIF_SIMD_TRACE_F

#if defined(XNIF_SIMD_AVX512F)
#undef XNIF_SIMD_AVX512F
#endif
#if defined(XNIF_SIMD_AVX2)
#undef XNIF_SIMD_AVX2
#endif
#if defined(XNIF_SIMD_NEON)
#undef XNIF_SIMD_NEON
#endif
#if defined(XNIF_SIMD_SSE2)
#undef XNIF_SIMD_SSE2
#endif

#ifdef __cplusplus
}
#endif

#endif
