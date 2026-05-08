#!/bin/bash

set -e
#set -x

cleanup() {
	umount "$work/merged" || true
	rm -rf "$work" || true
}

if [ "$EUID" -ne 0 ]; then
	exec sudo "$0" "$@"
fi

work="$(mktemp -d /tmp/root-overlay.XXXXXX)"
trap cleanup EXIT

for dir in upper work merged; do
	mkdir -p "$work/$dir"
done

mount -t overlay overlay \
	-o lowerdir=/,upperdir="$work/upper",workdir="$work/work" \
	"$work/merged"

systemd-nspawn \
	-D "$work/merged" \
	--as-pid2 \
	--bind="/run/user/$SUDO_UID" \
	--bind=/run/systemd/resolve \
	--bind=/tmp \
	--chdir="$PWD" \
	--hostname="$HOSTNAME" \
	--setenv="DISPLAY=$DISPLAY" \
	--setenv="WAYLAND_DISPLAY=$WAYLAND_DISPLAY" \
	--user="$SUDO_USER" \
	/bin/bash -l
