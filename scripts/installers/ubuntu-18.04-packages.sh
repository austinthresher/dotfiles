#!/bin/bash
apt-get update
apt-get install -y \
    vim tmux git make build-essential \
    python3 python3-pip graphviz doxygen cppcheck \
    libsdl2-dev libsdl2-ttf-dev tree indent \
    ncurses-term silversearcher-ag curl
