#!/bin/sh
#
# build chicken-grass from distribution files
#
# usage: bootstrap.sh [CC [OPTIONS ...]]


set -e
cflags="-Os -fomit-frame-pointer -fwrapv -fno-strict-aliasing -Ichicken"
ldflags="-lm"
cc=gcc
applyhack=""

case `uname -m` in
    x86*|i[3456]86)
	cflags="$cflags -DC_HACKED_APPLY"
	applyhack="bootstrap/apply-hack.S";;
esac

if test -n "$1"; then
    cc="$1"
    shift
fi

$cc -Ibootstrap $cflags \
  bootstrap/runtime.c \
  bootstrap/library.c \
  bootstrap/build-version.c \
  bootstrap/chicken-grass.c \
  bootstrap/numbers.c \
  $ldflags \
  $applyhack -o chicken-grass "$@"
