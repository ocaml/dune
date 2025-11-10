Test the error message when curl is needed but not installed.

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

Build the package in an environment without curl.
  $ PATH=$(dirname $(which dune)) build_pkg foo &> error.txt
  $ cat error.txt | grep "^Error: Program unzip not found in the tree or in PATH"
  [1]

