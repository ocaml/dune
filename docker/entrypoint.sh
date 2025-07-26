#!/usr/bin/env bash
set -e

USERNAME=opam

if [ -n "$HOST_UID" ] && [ -n "$HOST_GID" ]; then
    # Change UID/GID if different from current
    if [ "$(id -u $USERNAME)" != "$HOST_UID" ]; then
        usermod -u "$HOST_UID" $USERNAME
    fi
    if [ "$(id -g $USERNAME)" != "$HOST_GID" ]; then
        if ! getent group "$HOST_GID" >/dev/null; then
            groupmod -g "$HOST_GID" $USERNAME
        fi
    fi
fi

# Ensure home dir permissions are correct
chown -R $USERNAME:$USERNAME /home/$USERNAME/dune

# Switch to the user and run the CMD
exec su - $USERNAME -c "$*"
