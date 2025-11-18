Test the error message when unzip is needed but not installed.

  $ . ./helpers.sh

  $ make_lockdir

  $ mkdir -p .fakebin
  $ ln -s $(which dune) .fakebin/dune
  $ ln -s $(which sh) .fakebin/sh
  $ ln -s $(which cp) .fakebin/cp

  $ echo "random" >> test.txt.zip

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
  $ PATH=.fakebin build_pkg foo 2>&1 | grep '^Error:' -A 3
  Error: No program found to extract zip file. Tried:
  - unzip
  - bsdtar
  - tar

Build with only tar that doesn't work, not bsdtar or unzip, it should still fail to build

  $ cat > .fakebin/tar << 'EOF'
  > #!/usr/bin/env sh
  > case $1 in
  >  --version)
  >    echo "tar"
  >  ;;
  >  *)
  >    cp "$2" "$4/$(basename "${2%.zip}")"
  > esac
  > EOF
  $ chmod +x .fakebin/tar
  $ PATH=.fakebin build_pkg foo 2>&1 | grep '^Error:' -A 3
  Error: No program found to extract zip file. Tried:
  - unzip
  - bsdtar
  - tar

Build with only tar that works, not bsdtar or unzip, it should work

  $ cat > .fakebin/tar << 'EOF'
  > #!/usr/bin/env sh
  > case $1 in
  >  --version)
  >    echo "bsdtar"
  >  ;;
  >  *)
  >    cp "$2" "$4/$(basename "${2%.zip}")"
  > esac
  > EOF
  $ chmod +x .fakebin/tar
  $ PATH=.fakebin build_pkg foo

Build the package with bsdtar and tar. Now our fake bsdtar will get picked up
and built.

  $ cat > .fakebin/bsdtar << 'EOF'
  > #!/usr/bin/env sh
  > case $1 in
  >  --version)
  >    echo "bsdtar"
  >  ;;
  >  *)
  >    echo "I am faketar"
  >    cp "$2" "$4/$(basename "${2%.zip}")"
  > esac
  > EOF
  > chmod +x .fakebin/bsdtar
  $ PATH=.fakebin build_pkg foo

Build with unzip

  $ cat > .fakebin/unzip << 'EOF'
  > #!/usr/bin/env sh
  > case $1 in
  >  --version)
  >    echo "bsdtar"
  >  ;;
  >  *)
  >    echo "I am faketar"
  >    cp "$2" "$4/$(basename "${2%.zip}")"
  > esac
  > EOF
  $ chmod +x .fakebin/unzip
  $ PATH=fakebin:$(dirname $(which dune)) build_pkg foo
