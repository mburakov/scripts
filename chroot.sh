#!/bin/bash

set -e
set -x

cleanup() {
  umount "$ROOT/merged/dev" || true
  umount "$ROOT/merged/proc" || true
  umount "$ROOT/merged/sys" || true
  umount "$ROOT/merged/mnt" || true
  umount "$ROOT/merged" || true
  umount "$ROOT/lower" || true
}

if [ "$EUID" -ne 0 ]; then
  echo "Please run as root"
  exit
fi

if [ -z "$1" ]; then
  echo "Usage: $0 <rootfs.tar.gz>"
  exit 1
fi

DIR=$(dirname "$1")
ROOTFS=$(basename "$1")
IMAGE=${ROOTFS%.tar*}.sqfs
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
for dir in proc sys dev; do
  mount --bind "/$dir" "$ROOT/merged/$dir"
done

cp /etc/resolv.conf "$ROOT/merged/etc/resolv.conf"
mount --bind "$PWD" "$ROOT/merged/mnt"
chroot "$ROOT/merged" /bin/sh -l
