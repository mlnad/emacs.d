FROM archlinux:latest AS moyue-base
MAINTAINER Liu <liumiaogemini@foxmail.com>

ENV HOME=/root
ENV EMACS_DIR=/root/.emacs.d

RUN pacman -Syu --noconfirm && \
    pacman -S --noconfirm git gcc emacs && \
    pacman -Scc --noconfirm
