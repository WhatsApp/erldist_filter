# syntax=docker/dockerfile:1

# Copyright (c) Meta Platforms, Inc. and affiliates.
# Copyright (c) WhatsApp LLC
#
# This source code is licensed under the MIT license found in the
# LICENSE.md file in the root directory of this source tree.

FROM buildpack-deps:trixie AS otp-build

ARG OTP_VERSION="29.0.4"

RUN apt-get update \
    && apt-get install -y --no-install-recommends \
        clang \
        libclang-rt-dev \
        libssl-dev \
        llvm \
    && rm -rf /var/lib/apt/lists/*

RUN set -eux; \
    OTP_DOWNLOAD_URL="https://github.com/erlang/otp/releases/download/OTP-${OTP_VERSION}/otp_src_${OTP_VERSION}.tar.gz"; \
    OTP_DOWNLOAD_SHA256="67426425f0eed0dbd6c8cd3d500de4c47bbf1bbe337be0e2c13fc8f8f3e4997a"; \
    curl -fSL -o otp-src.tar.gz "$OTP_DOWNLOAD_URL"; \
    echo "$OTP_DOWNLOAD_SHA256  otp-src.tar.gz" | sha256sum -c -; \
    mkdir -p /usr/src/otp; \
    tar -xzf otp-src.tar.gz -C /usr/src/otp --strip-components=1; \
    rm otp-src.tar.gz; \
    cd /usr/src/otp; \
    ./otp_build autoconf; \
    gnuArch="$(dpkg-architecture --query DEB_HOST_GNU_TYPE)"; \
    sslLibSubdir="lib/$(dpkg-architecture --query DEB_HOST_MULTIARCH)"; \
    CC=clang CXX=clang++ ./configure \
        --build="$gnuArch" \
        --prefix=/opt/erlang \
        --disable-sctp \
        --without-javac \
        --without-odbc \
        --without-wx \
        --with-ssl=/usr \
        --with-ssl-lib-subdir="$sslLibSubdir"

RUN set -eux; \
    export ERL_TOP=/usr/src/otp; \
    cd "$ERL_TOP"; \
    make -j"$(nproc)"; \
    make -j"$(nproc)" TYPE=asan; \
    make install; \
    make release TYPE=asan RELEASE_ROOT=/opt/erlang/lib/erlang; \
    install -m 0644 erts/emulator/asan/suppress /opt/erlang/lib/erlang/asan.supp; \
    ASAN_OPTIONS=detect_leaks=0 /opt/erlang/bin/erl -emu_type asan -noshell \
        -eval 'asan = erlang:system_info(emu_type), halt().'

FROM buildpack-deps:trixie

ENV OTP_VERSION="29.0.4" \
    REBAR3_VERSION="3.27.0"

LABEL org.opencontainers.image.version=$OTP_VERSION

RUN apt-get update \
    && apt-get install -y --no-install-recommends \
        clang \
        libclang-rt-dev \
        libssl3t64 \
        llvm \
    && rm -rf /var/lib/apt/lists/*

COPY --from=otp-build /opt/erlang /opt/erlang
ENV PATH="/opt/erlang/bin:${PATH}"

RUN ASAN_OPTIONS=detect_leaks=0 erl -emu_type asan -noshell \
        -eval 'asan = erlang:system_info(emu_type), halt().'

RUN set -eux; \
    REBAR_DOWNLOAD_URL="https://github.com/erlang/rebar3/archive/${REBAR3_VERSION}.tar.gz"; \
    REBAR_DOWNLOAD_SHA256="985cae6e957334cfa549190b9f5efb9185c184a18fc181c87b8dde096ba79f38"; \
    mkdir -p /usr/src/rebar3-src; \
    curl -fSL -o rebar3-src.tar.gz "$REBAR_DOWNLOAD_URL"; \
    echo "$REBAR_DOWNLOAD_SHA256  rebar3-src.tar.gz" | sha256sum -c -; \
    tar -xzf rebar3-src.tar.gz -C /usr/src/rebar3-src --strip-components=1; \
    rm rebar3-src.tar.gz; \
    cd /usr/src/rebar3-src; \
    HOME=$PWD ./bootstrap; \
    install -m 0755 rebar3 /usr/local/bin/rebar3; \
    rm -rf /usr/src/rebar3-src

WORKDIR /project
COPY . .

ENV SANITIZERS_LSAN_SUPPRESSIONS=/opt/erlang/lib/erlang/asan.supp \
    SANITIZERS_RESTORE_BUILD=0

CMD ["bash", "justfiles/sanitizers/run.sh", "/project"]
