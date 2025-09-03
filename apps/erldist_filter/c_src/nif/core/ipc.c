/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * Copyright (c) WhatsApp LLC
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE.md file in the root directory of this source tree.
 */

#define _GNU_SOURCE /* See feature_test_macros(7) */
#include <fcntl.h>  /* O_CLOEXEC and O_NONBLOCK, if supported. */
#include <unistd.h>

#include "ipc.h"

#include <assert.h>
#include <limits.h> /* _POSIX_PATH_MAX, PATH_MAX */
#include <stdint.h>
#include <stdlib.h> /* abort */
#include <string.h> /* strrchr */
#include <stdio.h>
#include <errno.h>
#include <sys/stat.h>
#include <sys/types.h>

#ifdef __linux__
#include <sys/eventfd.h>
#endif

/* Macro Definitions */

#if EDOM > 0
#define IPC__ERRNO(x) (-(x))
#else
#define IPC__ERRNO(x) (x)
#endif

#if defined(_AIX) || defined(__APPLE__) || defined(__DragonFly__) || defined(__FreeBSD__) || defined(__FreeBSD_kernel__) ||        \
    defined(__linux__) || defined(__OpenBSD__) || defined(__NetBSD__)
#include <sys/ioctl.h>
#define ipc__nonblock ipc__nonblock_ioctl
#define IPC__NONBLOCK_IS_IOCTL 1
#else
#define ipc__nonblock ipc__nonblock_fcntl
#define IPC__NONBLOCK_IS_IOCTL 0
#endif

/* Static Declarations */

static void ipc_queue_message_destroy(ipc_queue_t *queue, ipc_message_t **messagep);

static int ipc__async_create(int *rfd, int *wfd);
static void ipc__async_send(int rfd, int wfd);
static int ipc__cloexec(int fd, int set);
static int ipc__close(int fd);
#if !defined(__linux__)
static int ipc__make_nonblock_pipe(int fds[2]);
#endif
#if IPC__NONBLOCK_IS_IOCTL
static int ipc__nonblock_ioctl(int fd, int set);
#else
static int ipc__nonblock_fcntl(int fd, int set);
#endif

/* Function Definitions */

int
ipc_queue_create(ipc_queue_t *queue, size_t capacity, char *mutex_name, ipc_queue_message_dtor_t message_dtor)
{
    (void)linklist_init_anchor(&queue->_link);
    (void)xnif_mutex_create(&queue->mutex, mutex_name);
    queue->message_dtor = message_dtor;
    queue->async.flag = (atomic_flag)ATOMIC_FLAG_INIT;
    queue->async.readfd = -1;
    queue->async.writefd = -1;
    if (ipc__async_create(&queue->async.readfd, &queue->async.writefd) != 0) {
        (void)xnif_mutex_destroy(&queue->mutex);
        (void)linklist_unlink(&queue->_link);
        return 0;
    }
    queue->capacity = capacity;
    queue->size = 0;
    queue->drop = 0;
    return 1;
}

void
ipc_queue_destroy(ipc_queue_t *queue)
{
    assert(linklist_is_empty(&queue->_link));
    (void)atomic_flag_test_and_set(&queue->async.flag);
    if (queue->async.readfd != -1) {
        (void)ipc__close(queue->async.readfd);
        queue->async.readfd = -1;
    }
    if (queue->async.writefd != -1) {
        (void)ipc__close(queue->async.writefd);
        queue->async.writefd = -1;
    }
    (void)xnif_mutex_destroy(&queue->mutex);
    (void)linklist_unlink(&queue->_link);
    return;
}

int
ipc_queue_clone_reader(ipc_queue_t *queue, int *readerp)
{
    int err;
    int newfd;
    (void)xnif_mutex_lock(&queue->mutex);
    newfd = dup(queue->async.readfd);
    if ((err = ipc__cloexec(newfd, 1)) != 0) {
        goto fail;
    }
    if ((err = ipc__nonblock(newfd, 1)) != 0) {
        goto fail;
    }
    (void)xnif_mutex_unlock(&queue->mutex);
    *readerp = newfd;
    return 1;

fail:
    (void)ipc__close(newfd);
    (void)xnif_mutex_unlock(&queue->mutex);
    return 0;
}

int
ipc_queue_recv(ipc_queue_t *queue, ipc_batch_t *batch)
{
    char buf[1024];
    ssize_t retval;

    assert(linklist_is_linked(&batch->_link) && linklist_is_empty(&batch->_link));

    for (;;) {
        retval = read(queue->async.readfd, buf, sizeof(buf));
        if (retval == sizeof(buf)) {
            continue;
        }
        if (retval != -1) {
            break;
        }
        if (errno == EAGAIN || errno == EWOULDBLOCK) {
            break;
        }
        if (errno == EINTR) {
            continue;
        }
        // should be unreachable
        abort();
    }

    (void)atomic_flag_clear_explicit(&queue->async.flag, memory_order_relaxed);
    (void)xnif_mutex_lock(&queue->mutex);
    (void)linklist_insert_list(&batch->_link, &queue->_link);
    batch->message_dtor = queue->message_dtor;
    batch->size = queue->size;
    batch->drop = queue->drop;
    queue->size = 0;
    queue->drop = 0;
    (void)xnif_mutex_unlock(&queue->mutex);
    return 1;
}

int
ipc_queue_send_multi(ipc_queue_t *queue, int messagec, ipc_message_t *messagev[])
{
    int i;
    linklist_t drop = {.next = NULL, .prev = NULL};
    int has_drop = 0;
    // assert(!linklist_is_linked(&message->_link));
    (void)xnif_mutex_lock(&queue->mutex);
    for (i = 0; i < messagec; i++) {
        (void)linklist_insert(&queue->_link, &messagev[i]->_link);
        queue->size += 1;
    }
    if (queue->capacity != 0 && queue->size > queue->capacity) {
        linklist_t *root = &queue->_link;
        linklist_t *node = root->next;
        linklist_t *temp = NULL;
        has_drop = 1;
        (void)linklist_init_anchor(&drop);
        while (queue->size > queue->capacity && node != root) {
            temp = node->next;
            (void)linklist_unlink(node);
            (void)linklist_insert(&drop, node);
            node = temp;
            queue->size -= 1;
            queue->drop += 1;
        }
    }
    (void)xnif_mutex_unlock(&queue->mutex);
    if (atomic_flag_test_and_set_explicit(&queue->async.flag, memory_order_relaxed) == false) {
        (void)ipc__async_send(queue->async.readfd, queue->async.writefd);
    }
    if (has_drop) {
        linklist_t *root = &drop;
        linklist_t *node = root->next;
        linklist_t *temp = NULL;
        while (node != root) {
            temp = node->next;
            (void)linklist_unlink(node);
            (void)ipc_queue_message_destroy(queue, (ipc_message_t **)&node);
            node = temp;
        }
        (void)linklist_unlink(&drop);
    }
    return 1;
}

int
ipc_queue_set_capacity(ipc_queue_t *queue, size_t new_capacity, size_t *old_capacity)
{
    size_t capacity;
    (void)xnif_mutex_lock(&queue->mutex);
    capacity = queue->capacity;
    queue->capacity = new_capacity;
    (void)xnif_mutex_unlock(&queue->mutex);
    if (old_capacity != NULL) {
        *old_capacity = capacity;
    }
    return 1;
}

void
ipc_queue_message_destroy(ipc_queue_t *queue, ipc_message_t **messagep)
{
    ipc_message_t *message = *messagep;
    *messagep = NULL;
    if (queue->message_dtor == NULL) {
        (void)ipc_message_destroy(message);
    } else {
        (void)queue->message_dtor(message);
    }
    return;
}

int
ipc__async_create(int *rfd, int *wfd)
{
    int pipefd[2];
    int err;

#if defined(__linux__)
    err = eventfd(0, EFD_CLOEXEC | EFD_NONBLOCK);
    if (err < 0) {
        return IPC__ERRNO(errno);
    }
    pipefd[0] = err;
    pipefd[1] = -1;
#else
    err = ipc__make_nonblock_pipe(pipefd);
    if (err < 0) {
        return err;
    }
#endif

    *rfd = pipefd[0];
    *wfd = pipefd[1];

    return 0;
}

void
ipc__async_send(int rfd, int wfd)
{
    const void *buf;
    ssize_t len;
    int fd;
    int retval;

    buf = "";
    len = 1;
    fd = wfd;

#if defined(__linux__)
    if (fd == -1) {
        // eventfd was used instead of pipe2
        static const uint64_t val = 1;
        buf = &val;
        len = sizeof(val);
        fd = rfd;
    }
#endif

    do {
        retval = write(fd, buf, len);
    } while (retval == -1 && errno == EINTR);

    if (retval == len) {
        return;
    }

    if (retval == -1) {
        if (errno == EAGAIN || errno == EWOULDBLOCK) {
            return;
        }
    }

    // should be unreachable
    abort();
}

int
ipc__cloexec(int fd, int set)
{
    int flags;
    int retval;

    flags = 0;
    if (set) {
        flags = FD_CLOEXEC;
    }

    do {
        retval = fcntl(fd, F_SETFD, flags);
    } while (retval == -1 && errno == EINTR);

    if (retval != 0) {
        return IPC__ERRNO(errno);
    }

    return 0;
}

int
ipc__close(int fd)
{
    return close(fd);
}

#if !defined(__linux__)
int
ipc__make_nonblock_pipe(int fds[2])
{
    int temp[2];
    int err = 0;
#if defined(__FreeBSD__) || defined(__linux__)
    if ((err = pipe2(temp, O_CLOEXEC | O_NONBLOCK)) != 0) {
        return IPC__ERRNO(errno);
    }
#else
    if (pipe(temp) != 0) {
        goto fail;
    }
    if ((err = ipc__cloexec(temp[0], 1)) != 0) {
        goto fail;
    }
    if ((err = ipc__cloexec(temp[1], 1)) != 0) {
        goto fail;
    }
    if ((err = ipc__nonblock(temp[0], 1)) != 0) {
        goto fail;
    }
    if ((err = ipc__nonblock(temp[1], 1)) != 0) {
        goto fail;
    }
#endif

    fds[0] = temp[0];
    fds[1] = temp[1];
    return 0;

#if !defined(__FreeBSD__) && !defined(__linux__)
fail:
    (void)ipc__close(temp[0]);
    (void)ipc__close(temp[1]);
    return err;
#endif
}
#endif

#if IPC__NONBLOCK_IS_IOCTL
int
ipc__nonblock_ioctl(int fd, int set)
{
    int retval;

    do {
        retval = ioctl(fd, FIONBIO, &set);
    } while (retval == -1 && errno == EINTR);

    if (retval != 0) {
        return IPC__ERRNO(errno);
    }

    return 0;
}
#else
int
ipc__nonblock_fcntl(int fd, int set)
{
    int flags;
    int retval;

    do {
        retval = fcntl(fd, F_GETFL);
    } while (retval == -1 && errno == EINTR);

    if (retval == -1) {
        return IPC__ERRNO(errno);
    }

    if (!!(retval & O_NONBLOCK) == !!set) {
        // Already in blocking/non-blocking mode, no need to use F_SETFL.
        return 0;
    }

    if (set) {
        flags = retval | O_NONBLOCK;
    } else {
        flags = retval & ~O_NONBLOCK;
    }

    do {
        retval = fcntl(fd, F_SETFL, flags);
    } while (retval == -1 && errno == EINTR);

    if (retval != 0) {
        return IPC__ERR(errno);
    }

    return 0;
}
#endif
