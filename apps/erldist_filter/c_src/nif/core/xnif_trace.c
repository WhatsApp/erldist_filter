/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * Copyright (c) WhatsApp LLC
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE.md file in the root directory of this source tree.
 */

#include <fcntl.h>
#include <stdio.h>
#include <time.h>
#include <unistd.h>

#include "xnif_trace.h"
#include "spinlock.h"

ERL_NIF_TERM
xnif_make_string_printf(ErlNifEnv *env, const char *format, ...)
{
    int ret;
    va_list arglist;
    va_start(arglist, format);
    ret = xnif_make_string_vprintf(env, format, arglist);
    va_end(arglist);
    return ret;
}

ERL_NIF_TERM
xnif_make_string_vprintf(ErlNifEnv *env, const char *format, va_list ap)
{
#define BUF_SZ 4096
    char buf[BUF_SZ];
    int res;
    size_t buf_len = 0;
    ERL_NIF_TERM buf_term;
    // size_t bin_len = 0;
    // unsigned char *bin_buf = NULL;
    // ERL_NIF_TERM bin_term;

    buf[0] = '\0';
    res = enif_vsnprintf(buf, BUF_SZ - 1, format, ap);
    XNIF_TRACE_F("enif_vsnprintf -> %d\n", res);
    XNIF_TRACE_F("string was %s\n", buf);
    if (res < 0) {
        return enif_raise_exception(env, enif_make_string(env, "Call to xnif_make_string_vprintf() failed", ERL_NIF_LATIN1));
    }
    if (res < BUF_SZ) {
        buf_len = (size_t)res;
    } else {
        buf_len = BUF_SZ;
    }
    buf_term = enif_make_string_len(env, buf, buf_len, ERL_NIF_LATIN1);
    // bin_buf = enif_make_new_binary(env, bin_len, &bin_term);
    // if (bin_buf == NULL) {
    //     XNIF_TRACE_F("UNABLE TO ALLOCATE BINBUF\n");
    //     return enif_raise_exception(env, enif_make_string(env, "Call to xnif_make_string_vprintf() failed", ERL_NIF_LATIN1));
    // }
    // (void)memcpy(bin_buf, buf, bin_len);
    XNIF_TRACE_F("output is: %T\n", buf_term);
    return buf_term;

    // if (res == 0) {
    //     return enif_make_string_len(env, buf, 0, ERL_NIF_LATIN1);
    // }
    // if (res < BUF_SZ) {
    //     return enif_make_string_len(env, buf, (size_t)(res - 1), ERL_NIF_LATIN1);
    // }
    // return enif_make_string_len(env, buf, BUF_SZ, ERL_NIF_LATIN1);
#undef BUF_SZ
}

#ifdef XNIF_TRACE
struct xnif_debug_file_s {
    spinlock_t mtx;
    FILE *stream;
};

static xnif_debug_file_t xnif_debug_file_default = {
    .mtx =
        {
            .value = ATOMIC_FLAG_INIT,
        },
    .stream = NULL,
};
xnif_debug_file_t *xnif_debug_file = &xnif_debug_file_default;

int
xnif_trace_vprintf(const char *format, va_list ap)
{
    int res = 0;
    (void)spinlock_lock(&xnif_debug_file->mtx);
#ifdef XNIF_TRACE_USE_STDERR
    if (xnif_debug_file->stream == NULL) {
        xnif_debug_file->stream = stderr;
    }
#else
    if (xnif_debug_file->stream == NULL) {
        static char suffix[256];
        static char filename[256];
        static pid_t pid;
        static time_t now;
        static struct tm *timenow;

        pid = getpid();
        now = time(NULL);
        timenow = gmtime(&now);
        (void)strftime(suffix, sizeof(suffix), "%Y-%m-%d_%H:%M:%S", timenow);
        (void)snprintf(filename, sizeof(filename), "/tmp/xnif-trace.%u.%s.log", pid, suffix);
        xnif_debug_file->stream = fopen(filename, "w");
    }
#endif
    res += enif_fprintf(xnif_debug_file->stream, "[%u.%p] ", getpid(), (void *)enif_thread_self());
    res += enif_vfprintf(xnif_debug_file->stream, format, ap);
    (void)fflush(xnif_debug_file->stream);
    (void)spinlock_unlock(&xnif_debug_file->mtx);
    return res;
}
#endif
