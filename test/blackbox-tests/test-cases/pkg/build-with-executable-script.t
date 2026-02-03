Shows what happens when Dune tries to build a package which runs run an
executable script.

Setup dune-project, dune-workspace, etc.

  $ make_lockdir

  $ mkdir scripts/

  $ cat > dune-project <<EOF
  > (lang dune 3.22)
  > EOF

  $ pkg() {
  > make_lockpkg $1 <<EOF
  > (build (run $2))
  > (version dev)
  > EOF
  > local files="${source_lock_dir}/$1.files"
  > local exec_path="${source_lock_dir}/$1.files/$2"
  > mkdir -p "${files}"
  > touch "${exec_path}"
  > chmod a+x "${exec_path}"
  > cat > "${exec_path}"
  > }

Simple executable

  $ pkg foo ./executable.sh<<EOF
  > #!/bin/sh
  > echo "Executable ran successfully!"
  > EOF
  $ dune build @pkg-install
  Executable ran successfully!

Simple executable with spaces in shebang line

  $ pkg foo ./executable.sh<<EOF
  > #! /bin/sh 
  > echo "Executable ran successfully (spaces in shebang line)!"
  > EOF
  $ dune build @pkg-install
  Executable ran successfully (spaces in shebang line)!

Executable with arg

  $ pkg foo ./executable.sh <<EOF
  > #!/bin/sh -v
  > echo "Executable ran successfully in verbose mode!"
  > EOF
  $ dune build @pkg-install
  Executable ran successfully in verbose mode!

Shebang with double spaces

  $ pkg foo ./executable.sh <<EOF
  > #!/bin/sh  -v
  > echo "Executable ran successfully in verbose mode; double spaces!"
  > EOF
  $ dune build @pkg-install
  Executable ran successfully in verbose mode; double spaces!

Executable as argument to env

  $ pkg foo ./executable.sh <<EOF
  > #!/usr/bin/env sh
  > echo "Executable ran successfully using sh!"
  > EOF
  $ dune build @pkg-install
  Executable ran successfully using sh!

Running using env -S

  $ pkg foo ./executable.sh <<EOF
  > #!/usr/bin/env -S sh -v
  > echo "Executable ran successfully using sh -v!"
  > EOF
  $ dune build @pkg-install
  Executable ran successfully using sh -v!

Running using env -S with spaces in shebang line

  $ pkg foo ./executable.sh <<EOF
  > #!  /usr/bin/env   -S  sh -v  
  > echo "Executable ran successfully using sh -v (with spaces in shebang line)!"
  > EOF
  $ dune build @pkg-install
  Executable ran successfully using sh -v (with spaces in shebang line)!

Running using sh binary

  $ pkg foo ./executable.sh <<EOF
  > echo "Executable ran successfully using sh binary!"
  > EOF
  $ make_lockpkg foo <<EOF
  > (build (run sh ./executable.sh))
  > (version dev)
  > EOF
  $ dune build @pkg-install
  Executable ran successfully using sh binary!
