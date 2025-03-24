Test the error message when curl is needed but not installed.

  $ . ./helpers.sh
  $ make_lockdir

  $ makepkg() {
  > make_lockpkg $1 <<EOF
  > (source
  >  (fetch
  >   (url "http://0.0.0.0:8000")))
  > (version dev)
  > EOF
  > }

  $ makepkg foo

Build the package in an environment without curl.
  $ PATH=$(dirname $(which dune)) build_pkg foo
  File "dune.lock/foo.pkg", line 3, characters 7-28:
  3 |   (url "http://0.0.0.0:8000")))
             ^^^^^^^^^^^^^^^^^^^^^
  Error: The program "curl" does not appear to be installed. Dune uses curl to
  download packages. Dune requires that the "curl" executable be located in one
  of the directories listed in the PATH variable.
  Hint: Install curl with your system package manager.
  [1]
