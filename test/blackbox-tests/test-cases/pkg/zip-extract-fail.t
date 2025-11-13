Test the error message when unzip is needed but not installed.

  $ . ./helpers.sh

  $ make_lockdir
  $ alias zip='cp'
  $ alias unzip='cp'
  $ alias bsdtar='cp'

  $ echo "random" >> test.txt
  $ zip test.txt bar.zip >> /dev/null

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
  Error: - No program found to extract zip file. Tried
         - unzip
         - bsdtar
         - tar

Build with only tar, not bsdtar or unzip, it should still fail to build

  $ PATH=$(dirname $(which dune)):$(dirname $(which tar)) build_pkg foo 2>&1 | grep '^Error:' -A 3
  Error: - No program found to extract zip file. Tried
         - unzip
         - bsdtar
         - tar

Build the package with bsdtar and tar, tar doesn't help but bsdtar should. Note
that however since we're aliasing unzip to cp, it won't do the extraction. But
that's ok because we just want to check if it's using the correct binary.

  $ PATH=$(dirname $(which dune)):$(dirname $(which tar)):$(dirname $(which bsdtar)) build_pkg foo 2>&1 | grep '^Error:'
  Error: failed to extract 'bar.zip'

Build with unzip. Same failure expected as above.

  $ PATH=$(dirname $(which dune)):$(dirname $(which unzip)) build_pkg foo 2>&1 | grep '^Error:'
  Error: failed to extract 'bar.zip'
