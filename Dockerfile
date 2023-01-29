FROM archlinux:latest AS emacs.d
    MAINTAINER Liu <liumiaogemini@foxmail.com>

RUN pacman -Syu --noconfirm && \
    pacman -S --noconfirm git vim gcc texinfo \
    clang emacs

EXPOSE 22
