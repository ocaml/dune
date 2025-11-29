Test that tar.gz extraction works with OpenBSD-style tar that requires -z flag.

  $ . ./helpers.sh

  $ make_lockdir

Set up our fake tar binary that requires -z flag for gzip decompression
(like OpenBSD's tar):

  $ mkdir -p .binaries
  $ cat > .binaries/bsdtar << 'EOF'
  > #!/usr/bin/env sh
  > # Fake tar that requires -z flag for gzip files (like OpenBSD tar)
  > has_z_flag=false
  > for arg in "$@"; do
  >   case "$arg" in
  >     -z) has_z_flag=true ;;
  >     *z*) has_z_flag=true ;;
  >   esac
  > done
  > # Check if we're extracting a .tar.gz without -z flag
  > for arg in "$@"; do
  >   case "$arg" in
  >     *.tar.gz|*.tgz)
  >       if [ "$has_z_flag" = "false" ]; then
  >         echo "tar: input compressed with gzip; use the -z option to decompress it." >&2
  >         exit 1
  >       fi
  >       ;;
  >   esac
  > done
  > # Otherwise succeed and create expected output
  > target=""
  > for i in "$@"; do
  >   if [ "$prev" = "-C" ]; then
  >     target="$i"
  >   fi
  >   prev="$i"
  > done
  > if [ -n "$target" ]; then
  >   mkdir -p "$target/extracted"
  >   echo "extracted content" > "$target/extracted/file.txt"
  > fi
  > EOF
  $ chmod +x .binaries/bsdtar

Set up a folder that we will inject as fake PATH:

  $ mkdir -p .fakebin
  $ ln -s $(which dune) .fakebin/dune
  $ ln -s $(which sh) .fakebin/sh
  $ ln -s $(which mkdir) .fakebin/mkdir
  $ ln -s $(which echo) .fakebin/echo
  $ cp .binaries/bsdtar .fakebin/tar

Create a fake tar.gz file:

  $ echo "fake tarball content" > test.tar.gz

  $ makepkg() {
  > make_lockpkg $1 <<EOF
  > (source
  >  (fetch
  >   (url "file://$(pwd)/test.tar.gz")))
  > (version dev)
  > EOF
  > }

  $ makepkg foo

Build the package - this should work because we pass -z flag for .tar.gz:

  $ (PATH=.fakebin build_pkg foo)
