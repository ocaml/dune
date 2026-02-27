Test that demonstrates the issue with OpenBSD-style tar (#10123).

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
  >     # Dune passes: xf archive -C target (or xzf archive -C target with fix)
  >     flags="$1"
  >     archive="$2"
  >     target="$4"
  >     # Require correct flag for compressed archives
  >     case "$archive" in
  >       *.tar.gz|*.tgz)
  >         echo "$flags" | grep -q 'z' || { echo "tar: Cannot open: compressed archive requires -z flag" >&2; exit 1; }
  >         ;;
  >       *.tar.bz2|*.tbz)
  >         echo "$flags" | grep -q 'j' || { echo "tar: Cannot open: compressed archive requires -j flag" >&2; exit 1; }
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

Test with a .tar.gz file - fails because dune doesn't pass -z flag:

  $ echo "fake tarball" > test.tar.gz

  $ make_lockpkg foo <<EOF
  > (source
  >  (fetch
  >   (url "file://$PWD/test.tar.gz")))
  > (version dev)
  > EOF

  $ PATH=.fakebin build_pkg foo 2>&1 | grep -A2 '^Error'
  Error: failed to extract 'test.tar.gz'
  Reason: 'tar' failed with non-zero exit code '1' and output:
  - tar: Cannot open: compressed archive requires -z flag
  [1]

Test with a .tbz file - fails because dune doesn't pass -j flag:

  $ echo "fake tarball" > test.tbz

  $ make_lockpkg bar <<EOF
  > (source
  >  (fetch
  >   (url "file://$PWD/test.tbz")))
  > (version dev)
  > EOF

  $ PATH=.fakebin build_pkg bar 2>&1 | grep -A2 '^Error'
  Error: failed to extract 'test.tbz'
  Reason: 'tar' failed with non-zero exit code '1' and output:
  - tar: Cannot open: compressed archive requires -j flag
  [1]
