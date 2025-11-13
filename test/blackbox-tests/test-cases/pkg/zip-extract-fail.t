Test the error message when unzip is needed but not installed.

  $ . ./helpers.sh

  $ make_lockdir

  $ echo "random" >> test.txt
  $ zip bar.zip test.txt >> /dev/null

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
  $ PATH=$(dirname $(which dune)) build_pkg foo 2>&1 | grep '^Error:' -A 3
  Error: No program found to extract zip file. Tried
  unzip
  bsdtar
  tar


Build with only tar, not bsdtar or unzip, it should still fail to build

  $ PATH=$(dirname $(which dune)):$(dirname $(which tar)) build_pkg foo 2>&1 | grep '^Error:' -A 3
  Error: No program found to extract zip file. Tried
  unzip
  bsdtar
  tar

Build the package with bsdtar and tar, tar doesn't help but bsdtar should

  $ PATH=$(dirname $(which dune)):$(dirname $(which tar)):$(dirname $(which bsdtar)) build_pkg foo

Build with unzip

  $ PATH=$(dirname $(which dune)):$(dirname $(which unzip)) build_pkg foo
