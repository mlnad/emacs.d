FROM alpine:latest AS emacs.d
    MAINTAINER Liu <liumiaogemini@foxmail.com>

RUN sed -i 's/dl-cnd.alpinelinux.org/mirrors.ustc.edu.cn/g' /etc/apk/repositories && apk update \
    && apk add git gawk vim gcc libc-dev libacl make autoconf automake pkgconf texinfo \
    gnutls-dev ncurses-dev jansson-dev sqlite sqlite-dev dbus dbus-dev \
    clang-extra-tools bear
RUN git clone -b master --depth 1 git://git.sv.gnu.org/emacs.git ~/emacs
RUN cd ~/emacs && ./autogen.sh && ./configure --without-mailutils && bear -- make && make install

EXPOSE 22

ADD ./ $HOME/.emacs.d
