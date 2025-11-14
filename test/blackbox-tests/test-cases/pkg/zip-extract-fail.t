Test the error message when unzip is needed but not installed.

  $ . ./helpers.sh

  $ make_lockdir

  $ mkdir -p fakebin
  $ cat > fakebin/zip << 'EOF'
  > #!/usr/bin/env sh
  > cp "$@"
  > EOF
  $ chmod +x fakebin/zip

  $ echo "random" >> test.txt
  $ PATH=fakebin:$PATH zip test.txt bar.zip >> /dev/null

  $ makepkg() {
  > make_lockpkg $1 <<EOF
  > (source
  >  (fetch
  >   (url "file://$(pwd)/bar.zip")))
  > (version dev)
  > EOF
  > }

  $ makepkg foo

Build the package in an environment without unzip, or tar, or bsdtar.

  $ PATH=fakebin:$(dirname $(which dune)) build_pkg foo 2>&1 | grep '^Error:' -A 3
  Error: - No program found to extract zip file. Tried:
         - unzip
         - bsdtar
         - tar

Build with only tar, not bsdtar or unzip, it should still fail to build

  $ cat > fakebin/tar << 'EOF'
  > #!/usr/bin/env sh
  > cp "$@"
  > EOF
  $ chmod +x fakebin/tar
  $ PATH=fakebin:$(dirname $(which dune)) build_pkg foo 2>&1 | grep '^Error:' -A 3
  Error: - No program found to extract zip file. Tried:
         - unzip
         - bsdtar
         - tar

Build the package with bsdtar and tar, tar doesn't help but bsdtar should. Note
that however since we're aliasing bsdtar to cp, it will fail. This is because it
checks if `bsdtar --version` returns the string bsdtar and in this case it
doesn't. But that's ok because we just want to check if it's using the correct
binary.

  $ cat > fakebin/bsdtar << 'EOF'
  > #!/usr/bin/env sh
  > cp "$@"
  > EOF
  $ chmod +x fakebin/bsdtar
  $ PATH=fakebin:$(dirname $(which dune)) build_pkg foo 2>&1 | grep '^Error:' -A 3
  Error: - No program found to extract zip file. Tried:
         - unzip
         - bsdtar
         - tar

Build with unzip. Same failure expected as above.

  $ cat > fakebin/unzip << 'EOF'
  > #!/usr/bin/env sh
  > cp "$@"
  > EOF
  $ chmod +x fakebin/unzip
  $ PATH=fakebin:$(dirname $(which dune)) build_pkg foo 2>&1 | grep '^Error:'
  Error: failed to extract 'bar.zip'
