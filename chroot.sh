#!/bin/bash

set -e
set -x

cleanup() {
  umount "$ROOT/merged/mnt" || true
  umount "$ROOT/merged" || true
  umount "$ROOT/lower" || true
}

if [ "$#" -lt "1" ]; then
  echo "Usage: $0 <rootfs.tar.gz> [command [args...]]"
  exit 1
fi

if [ "$EUID" -ne 0 ]; then
  exec sudo "$0" "$@"
fi

DIR=$(dirname "$1")
ROOTFS=$(basename "$1")
IMAGE=${ROOTFS%.tar*}.sqfs
shift

if [ ! -f "$DIR/$IMAGE" ]; then
  echo "Creating SquashFS image..."
  bsdtar -s '|^\./||' -cf - "@$DIR/$ROOTFS" |
    mksquashfs - "$DIR/$IMAGE" -tar -noappend
fi

ROOT=/tmp/${IMAGE%.sqfs}
trap cleanup EXIT

for dir in lower upper work merged; do
  mkdir -p "$ROOT/$dir"
done

mount "$DIR/$IMAGE" "$ROOT/lower"
mount -t overlay overlay \
  -o lowerdir="$ROOT/lower",upperdir="$ROOT/upper",workdir="$ROOT/work" \
  "$ROOT/merged"

mount --bind "$PWD" "$ROOT/merged/mnt"
arch-chroot "$ROOT/merged" "$@"
