Test the error message when tar is not available

  $ make_lockdir

Set up our fake decompressor binaries, they all just copy the file to the
expected location:

  $ mkdir -p .binaries
  $ cat > .binaries/gnutar << 'EOF'
  > #!/usr/bin/env sh
  > case "$1" in
  >  --version)
  >    echo "tar"
  >  ;;
  >  *)
  >    cp "$2" "$4/$(basename "${2%.tar}")"
  > esac
  > EOF
  $ chmod +x .binaries/gnutar
  $ cat > .binaries/bsdtar << 'EOF'
  > #!/usr/bin/env sh
  > case "$1" in
  >  --version)
  >    echo "bsdtar"
  >  ;;
  >  *)
  >    cp "$2" "$4/$(basename "${2%.tar}")"
  > esac
  > EOF
  $ chmod +x .binaries/bsdtar

Set up a folder that we will inject as fake PATH:

  $ mkdir -p .fakebin
  $ ln -s $(which dune) .fakebin/dune
  $ ln -s $(which sh) .fakebin/sh
  $ ln -s $(which cp) .fakebin/cp
  $ show_path() {
  >   ls .fakebin | sort | xargs
  > }

Our TAR file is not compressed (as the extraction is just a copy),
but does have a .tar suffix:

  $ echo "random" > test.txt.tar

  $ makepkg() {
  > make_lockpkg $1 <<EOF
  > (source
  >  (fetch
  >   (url "file://$(pwd)/test.txt.tar")))
  > (version dev)
  > EOF
  > }

  $ makepkg foo

Build the package in an environment without tar, gtar or bsdtar.

  $ show_path
  cp dune sh
  $ PATH=.fakebin build_pkg foo 2>&1 | grep '^Error:' -A 3
  Error: No program found to extract tar file. Tried:
  - tar
  - bsdtar
  - gtar
  [1]

Build with only GNU tar as `tar`:

  $ cp .binaries/gnutar .fakebin/tar
  $ show_path
  cp dune sh tar
  $ PATH=.fakebin build_pkg foo

Build with bsdtar that can extract TAR files:

  $ rm .fakebin/tar
  $ cp .binaries/bsdtar .fakebin/tar
  $ show_path
  cp dune sh tar
  $ PATH=.fakebin build_pkg foo

Build the package with bsdtar and tar. Now our fake bsdtar will get picked up
and used to extract:

  $ rm .fakebin/tar
  $ ln -s .binaries/gnutar .fakebin/tar
  $ ln -s .binaries/bsdtar .fakebin/bsdtar
  $ show_path
  bsdtar cp dune sh tar
  $ PATH=.fakebin build_pkg foo

Build the package with gtar:

  $ rm .fakebin/{bsd,}tar
  $ ln -s .binaries/gnutar .fakebin/gtar
  $ ln -s .binaries/bsdtar .fakebin/tar
  $ show_path
  cp dune gtar sh tar
  $ PATH=.fakebin build_pkg foo

Build with gtar only:

  $ rm .fakebin/tar
  $ show_path
  cp dune gtar sh
  $ PATH=.fakebin build_pkg foo
