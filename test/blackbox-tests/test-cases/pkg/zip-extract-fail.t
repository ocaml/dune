Test the error message when unzip is needed but not installed.

  $ . ./helpers.sh

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
  >    cp "$2" "$4/$(basename "${2%.zip}")"
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
  >    cp "$2" "$4/$(basename "${2%.zip}")"
  > esac
  > EOF
  $ chmod +x .binaries/bsdtar
  $ cat > .binaries/unzip << 'EOF'
  > #!/usr/bin/env sh
  > cp "$2" "$4/$(basename "${2%.zip}")"
  > EOF
  $ chmod +x .binaries/unzip

Set up a folder that we will inject as fake PATH:

  $ mkdir -p .fakebin
  $ ln -s $(which dune) .fakebin/dune
  $ ln -s $(which sh) .fakebin/sh
  $ ln -s $(which cp) .fakebin/cp
  $ show_path() {
  >   ls .fakebin | sort | xargs
  > }

Our "compressed" ZIP file is not compressed (as the extraction is just a copy),
but does have a .zip suffix:

  $ echo "random" > test.txt.zip

  $ makepkg() {
  > make_lockpkg $1 <<EOF
  > (source
  >  (fetch
  >   (url "file://$(pwd)/test.txt.zip")))
  > (version dev)
  > EOF
  > }

  $ makepkg foo

Build the package in an environment without unzip, or tar, or bsdtar.

(NOTE: We wrap `(PATH=.fakebin foo)` in parens, otherwise the value of the PATH
variable can escape to subseqent shell invocations on MacOS.)

  $ show_path
  cp dune sh
  $ (PATH=.fakebin build_pkg foo 2>&1 | grep '^Error:' -A 3)
  Error: No program found to extract zip file. Tried:
  - unzip
  - bsdtar
  - tar

Build with only GNU tar that can't extract ZIP archives:

  $ ln -s .binaries/gnutar .fakebin/tar
  $ show_path
  cp dune sh tar
  $ (PATH=.fakebin build_pkg foo 2>&1 | grep '^Error:' -A 3)
  Error: No program found to extract zip file. Tried:
  - unzip
  - bsdtar
  - tar

Build with bsdtar that can extract ZIP archives, without unzip. It should work:

  $ rm .fakebin/tar
  $ cp .binaries/bsdtar .fakebin/tar
  $ show_path
  cp dune sh tar
  $ (PATH=.fakebin build_pkg foo)

Build the package with bsdtar and tar. Now our fake bsdtar will get picked up
and used to extract:

  $ rm .fakebin/tar
  $ ln -s .binaries/gnutar .fakebin/tar
  $ ln -s .binaries/bsdtar .fakebin/bsdtar
  $ show_path
  bsdtar cp dune sh tar
  $ (PATH=.fakebin build_pkg foo)

Build with unzip only:

  $ ln -s .binaries/unzip .fakebin/unzip
  $ rm .fakebin/bsdtar .fakebin/tar
  $ show_path
  cp dune sh unzip
  $ (PATH=.fakebin build_pkg foo)
