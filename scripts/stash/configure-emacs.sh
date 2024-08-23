#!/bin/bash

# This was for emacs-29.4 on ubuntu 22.04 LTS

sudo apt install \
    libgtk-3-dev libgtk-3-0 \
    libtool libtool-bin libxpm-dev libgif-dev gnutls-dev \
    libjansson4 libjansson-dev librsvg2-dev \
    libtree-sitter-dev libgccjit-11-dev \
    libgpm-dev libsqlite3-dev libotf-dev libxft-dev \
    libwxgtk3.0-gtk3-dev libwebkit2gtk-4.1-dev

# optional: --without-toolkit-scroll-bars
./configure --with-native-compilation --with-xwidgets
