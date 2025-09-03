# syntax=docker/dockerfile:1

# Copyright (c) Meta Platforms, Inc. and affiliates.
# Copyright (c) WhatsApp LLC
#
# This source code is licensed under the MIT license found in the
# LICENSE.md file in the root directory of this source tree.

# Use an official Python image.
FROM python:3.13-alpine

# Install apk dependencies.
RUN apk add --no-cache bash clang-extra-tools
RUN apk add --no-cache -X https://dl-cdn.alpinelinux.org/alpine/edge/community just

# Set the working directory.
RUN mkdir -p /project
WORKDIR /project
COPY ./justfile .

# Setup Python.
COPY ./justfiles/python.just ./justfiles/python.just
ENV PYTHONDONTWRITEBYTECODE=1
ENV PYTHONUNBUFFERED=1
RUN just setup-python
ENV POETRY_VIRTUALENVS_IN_PROJECT=true
