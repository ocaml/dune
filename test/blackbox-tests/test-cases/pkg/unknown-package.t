Try to build a package that doesn't exist

  $ . ./helpers.sh

  $ make_lockdir
  $ build_pkg fakepkg
  Error: Unknown package "fakepkg"
  [1]
