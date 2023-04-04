#!/bin/sh
set -euf

# shellcheck disable=SC2154
echo "
=============
build-test.sh
=============
.
------
Matrix
------
dkml_host_abi=$dkml_host_abi
abi_pattern=$abi_pattern
opam_root=$opam_root
exe_ext=${exe_ext:-}
.
"

# Set project directory
if [ -n "${CI_PROJECT_DIR:-}" ]; then
    PROJECT_DIR="$CI_PROJECT_DIR"
elif [ -n "${PC_PROJECT_DIR:-}" ]; then
    PROJECT_DIR="$PC_PROJECT_DIR"
elif [ -n "${GITHUB_WORKSPACE:-}" ]; then
    PROJECT_DIR="$GITHUB_WORKSPACE"
else
    PROJECT_DIR="$PWD"
fi
if [ -x /usr/bin/cygpath ]; then
    PROJECT_DIR=$(/usr/bin/cygpath -au "$PROJECT_DIR")
fi

# PATH. Add opamrun
export PATH="$PROJECT_DIR/.ci/sd4/opamrun:$PATH"

# Initial Diagnostics (optional but useful)
opamrun switch
opamrun list
opamrun var
opamrun config report
opamrun option
opamrun exec -- ocamlc -config

# Update
opamrun update

# ------------------
# Dune's build logic.
#   For now it mimics the "build" job in .github/workflows/workflow.yml
# ------------------

echo ======== Unpin Dune from DKML defaults
opamrun pin list
opamrun pin remove dune --no-action
opamrun pin remove dune-configurator --no-action

echo ======== Set git user
git config --global user.name 'github-actions[bot]'
git config --global user.email 'github-actions[bot]@users.noreply.github.com'

echo ======== Install ocamlfind-secondary and ocaml-secondary-compiler, if needed
opamrun install ./opam/dune.opam --deps-only --with-test

case "$dkml_host_abi" in
darwin_*)
    echo ======== Install system deps on macOS
    brew install coreutils
    ;;
esac

# dune doesn't have any additional dependencies so we can build it right
# away this makes it possible to see build errors as soon as possible
echo ======== Build boot dune.exe
opamrun exec -- make _boot/dune.exe

echo ======== Ensure Dune can build itself
opamrun exec -- make bootstrap

case "$dkml_host_abi" in
windows_*)
    # Mitigate bug in opam:
    #
    #    #=== ERROR while compiling dune.3.6.0 =========================================#
    #    # path        Z:\source\dune
    #    # command     C:\Users\beckf\AppData\Local\Programs\DiskuvOCaml\tools\MSYS2\usr\bin\cp.exe -PRp Z:\source\dune\.ci\o\dkml\.opam-switch\sources\dune.3.6.0 Z:\source\dune\.ci\o\dkml\.opam-switch\build\dune.3.6.0
    #    # exit-code   1
    #    # env-file    Z:\source\dune\.ci\o\log\log-685364-345b91.env
    #    # output-file Z:\source\dune\.ci\o\log\log-685364-345b91.out
    #    ### output ###
    #    # /usr/bin/cp: cannot create regular file 'Z:\source\dune\.ci\o\dkml\.opam-switch\build\dune.3.6.0/dune': File exists
    #
    # Basically the 'cp' fallback logic in opam's src/core/opamSystem.ml:copy_dir is broken.
    # Using 'rsync' avoids that, so install it on MSYS2.
    if command -v pacman; then
        echo ======== Install rsync on Win32
        pacman --sync --needed --noconfirm rsync
    fi

    echo ======== Install deps on Win32
    opamrun install ./opam/dune-configurator.opam --deps-only --with-test --yes

    echo ======== Run test suite on Win32
    opamrun exec -- make test-windows
    ;;
*)
    echo ======== Install deps on Unix
    opamrun install . --deps-only --with-test --yes

    # Remove DKML pins (keep alphabetical; the pins will eventually be upstreamed in DKML)
    opamrun pin remove base --no-action
    opamrun pin remove core_kernel --no-action
    opamrun pin remove ocamlformat --no-action
    opamrun pin remove ppx_expect --no-action
    opamrun pin remove time_now --no-action

    opamrun exec -- make dev-deps

    echo ======== Run test suite on Unix
    opamrun exec -- make test
esac

echo ======== Build configurator
opamrun install ./opam/dune-configurator.opam --yes
