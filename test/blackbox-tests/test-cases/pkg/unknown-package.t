Try to build a package that doesn't exist

  $ . ./helpers.sh

  $ make_lockdir
  $ build_pkg fakepkg
  Error: The project does not depend on the package "fakepkg".
  [1]
