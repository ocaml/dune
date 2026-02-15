Test that OpenBSD-style tar works when dune passes explicit flags (#10123).

Unlike GNU tar and BSD tar (libarchive), OpenBSD tar requires explicit
flags for compressed archives: -z for gzip, -j for bzip2, etc.

  $ make_lockdir

Set up a fake tar that behaves like OpenBSD tar:
- Returns generic version (not "bsdtar" or "GNU tar")
- Requires explicit decompression flags

  $ mkdir -p .binaries
  $ cat > .binaries/openbsd-tar << 'EOF'
  > #!/usr/bin/env sh
  > case "$1" in
  >   --version)
  >     echo "tar (OpenBSD)"
  >     ;;
  >   *)
  >     # Dune passes: -x -z -f archive -C target (or -x -j -f ... for bzip2)
  >     # $4 = archive, $6 = target
  >     archive="$4"
  >     target="$6"
  >     # Require correct flag for compressed archives
  >     case "$archive" in
  >       *.tar.gz|*.tgz)
  >         echo "$@" | grep -q '\-z' || { echo "tar: Cannot open: compressed archive requires -z flag" >&2; exit 1; }
  >         ;;
  >       *.tar.bz2|*.tbz)
  >         echo "$@" | grep -q '\-j' || { echo "tar: Cannot open: compressed archive requires -j flag" >&2; exit 1; }
  >         ;;
  >     esac
  >     # Success - create fake extracted content
  >     mkdir -p "$target/pkg-content"
  >     echo "extracted" > "$target/pkg-content/file.txt"
  >     ;;
  > esac
  > EOF
  $ chmod +x .binaries/openbsd-tar

Set up fake PATH with only our OpenBSD-style tar:

  $ mkdir -p .fakebin
  $ ln -s $(which dune) .fakebin/dune
  $ ln -s $(which sh) .fakebin/sh
  $ ln -s $(which mkdir) .fakebin/mkdir
  $ ln -s $(which echo) .fakebin/echo
  $ ln -s $(which grep) .fakebin/grep
  $ ln -s ../.binaries/openbsd-tar .fakebin/tar

Test with a .tar.gz file:

  $ echo "fake tarball" > test.tar.gz

  $ make_lockpkg foo <<EOF
  > (source
  >  (fetch
  >   (url "file://$PWD/test.tar.gz")))
  > (version dev)
  > EOF

  $ PATH=.fakebin build_pkg foo

Verify that -z flag was passed:

  $ dune trace cat | jq -c 'include "dune"; processes | select(.args.prog | contains("tar")) | .args.process_args | map(if (startswith("/") or startswith("_build")) then "PATH" else . end)'
  ["--version"]
  ["-x","-z","-f","PATH","-C","PATH"]

Test with a .tbz file (bzip2 compressed):

  $ echo "fake tarball" > test.tbz

  $ make_lockpkg bar <<EOF
  > (source
  >  (fetch
  >   (url "file://$PWD/test.tbz")))
  > (version dev)
  > EOF

  $ PATH=.fakebin build_pkg bar

Verify that -j flag was passed:

  $ dune trace cat | jq -c 'include "dune"; processes | select(.args.prog | contains("tar")) | .args.process_args | map(if (startswith("/") or startswith("_build")) then "PATH" else . end)'
  ["--version"]
  ["-x","-j","-f","PATH","-C","PATH"]
