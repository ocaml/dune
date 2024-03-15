We set the PATH with (exported_env ..) and this should be reflected when
looking up binaries in the workspace.

  $ . ./helpers.sh

  $ make_lockdir

  $ bin=foobarbin

  $ mkdir _bin
  $ cat >_bin/$bin <<EOF
  > #!/usr/bin/env sh
  > echo foo
  > EOF
  $ chmod +x _bin/$bin

  $ make_lockpkg <<EOF
  > (version 1.0.0)
  > (exported_env
  >  (:= PATH $PWD/_bin))
  > EOF

  $ cat >dune-project <<EOF
  > (lang dune 3.13)
  > EOF

  $ cat >dune <<EOF
  > (rule
  >  (alias foo)
  >  (action (run %{bin:$bin})))
  > EOF

  $ dune build @foo
  File "dune", line 3, characters 14-30:
  3 |  (action (run %{bin:foobarbin})))
                    ^^^^^^^^^^^^^^^^
  Error: Program foobarbin not found in the tree or in PATH
   (context: default)
  [1]

  $ PATH=$PWD/_bin:$PATH $bin
  foo
