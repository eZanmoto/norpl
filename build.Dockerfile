# Copyright 2021 Sean Kelleher. All rights reserved.
# Use of this source code is governed by an MIT
# licence that can be found in the LICENCE file.

FROM rust:1.52.1-slim-bullseye

RUN \
    rustup component add \
        clippy

RUN \
    apt-get update \
    && apt-get install \
        --assume-yes \
        make \
        python3 \
        python3-pip \
        wget

RUN \
    wget \
        https://github.com/eZanmoto/dpnd/releases/download/v0.1.14/dpnd-v0.1.14-x86_64-unknown-linux-gnu.tar.gz \
        --output-document=/tmp/dpnd.tar.gz \
    && tar \
        --extract \
        --directory=/tmp \
        --file=/tmp/dpnd.tar.gz \
    && mv \
        /tmp/dpnd-v0.1.14-x86_64-unknown-linux-gnu \
        /usr/local/bin/dpnd \
    && pip3 install \
        comment-style===0.1.0
