# MoYu Emacs base image.
#
# One Dockerfile, one package set, two distributions. Select the
# distribution through the BASE_IMAGE build argument:
#
#   docker build -t moyue-base:ubuntu --build-arg BASE_IMAGE=ubuntu:latest .
#   docker build -t moyue-base:alpine --build-arg BASE_IMAGE=alpine:latest .
#
# The image only contains the tools `moyue install' / `moyue doctor' need,
# so the configuration itself is never baked in: it is mounted at run time.
ARG BASE_IMAGE=ubuntu:latest

FROM ${BASE_IMAGE} AS moyue-base
LABEL maintainer="Liu <liumiaogemini@foxmail.com>"

# Re-declared after FROM so the value is visible inside the build stage.
ARG BASE_IMAGE

ENV HOME=/root \
    EMACS_DIR=/root/.emacs.d

# Install the minimal toolchain:
#   emacs          the editor itself (no-X build)
#   git            package archives and tree-sitter grammar checkouts
#   gcc/make       native compilation and tree-sitter grammar builds
#   curl/unzip     `moyue install TOOL' downloads with one of them and
#                  unpacks clangd's release with the other
#   ca-certificates  HTTPS access to ELPA/MELPA
RUN case "${BASE_IMAGE}" in \
      ubuntu*) \
        export DEBIAN_FRONTEND=noninteractive && \
        apt-get update && \
        apt-get install -y --no-install-recommends \
          ca-certificates curl git gcc make libc6-dev emacs-nox unzip && \
        rm -rf /var/lib/apt/lists/* ;; \
      alpine*) \
        apk add --no-cache \
          ca-certificates curl git build-base emacs-nox unzip && \
        rm -rf /var/cache/apk/* ;; \
      *) \
        echo "moyue: unsupported BASE_IMAGE '${BASE_IMAGE}'" >&2; \
        echo "moyue: supported images: ubuntu:latest alpine:latest" >&2; \
        exit 1 ;; \
    esac
