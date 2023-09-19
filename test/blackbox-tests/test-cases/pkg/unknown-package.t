Try to build a package that doesn't exist

  $ . ./helpers.sh

  $ mkdir dune.lock
  $ cat >dune.lock/lock.dune <<EOF
  > (lang package 0.1)
  > EOF
  $ build_pkg fakepkg
  Error: Unknown package "fakepkg"
  [1]
