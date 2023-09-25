Check that Cram tests run without shell option specified

  $ echo '(lang dune 3.12)' > dune-project

  $ cat > foo.t <<EOF
  >   $ echo "foo from foo.t"
  > EOF

  $ dune runtest --auto-promote
  File "foo.t", line 1, characters 0-0:
  Error: Files _build/default/foo.t and _build/default/foo.t.corrected differ.
  Promoting _build/default/foo.t.corrected to foo.t.
  [1]

  $ dune runtest -f
  $ cat foo.t
    $ echo "foo from foo.t"
    foo from foo.t

Prepare custom and mock shells for following tests

  $ cat > dune <<EOF
  > (executables (names sh bash custom_shell))
  > (rule (mode (promote (until-clean))) (action (copy %{dep:./sh.exe} sh)))
  > (rule (mode (promote (until-clean))) (action (copy %{dep:./bash.exe} bash)))
  > EOF

  $ cat > sh.ml <<EOF
  > print_endline "sh.ml"
  > EOF

  $ cat > bash.ml <<EOF
  > print_endline "bash.ml"
  > EOF

  $ cat > custom_shell.ml <<EOF
  > print_endline "custom_shell.ml"
  > EOF

  $ dune build
  $ ls
  _build
  bash
  bash.ml
  custom_shell.ml
  dune
  dune-project
  foo.t
  sh
  sh.ml

  $ ./sh
  sh.ml
  $ ./bash
  bash.ml

Check that shell option unset uses `env sh`

  $ cat > dune <<EOF
  > (cram (deps sh))
  > EOF
  $ PATH=$PWD:$PATH

  $ cat > foo.t <<EOF
  >   $ echo "foo from foo.t"
  > EOF

  $ dune runtest --auto-promote
  sh.ml
  File "foo.t", line 1, characters 0-0:
  Error: Files _build/default/foo.t and _build/default/foo.t.corrected differ.
  Promoting _build/default/foo.t.corrected to foo.t.
  [1]

  $ dune runtest -f
  sh.ml
  $ cat foo.t
    $ echo "foo from foo.t"
    ***** UNREACHABLE *****

Check that shell = :system uses `env sh`

  $ cat > dune <<EOF
  > (cram (deps sh) (shell :system))
  > EOF
  $ PATH=$PWD:$PATH

  $ cat > foo.t <<EOF
  >   $ echo "foo from foo.t"
  > EOF

  $ dune runtest --auto-promote
  sh.ml
  File "foo.t", line 1, characters 0-0:
  Error: Files _build/default/foo.t and _build/default/foo.t.corrected differ.
  Promoting _build/default/foo.t.corrected to foo.t.
  [1]

  $ dune runtest -f
  sh.ml
  $ cat foo.t
    $ echo "foo from foo.t"
    ***** UNREACHABLE *****

Check that shell = bash uses `env bash`

  $ cat > dune <<EOF
  > (cram (deps bash) (shell bash))
  > EOF
  $ PATH=$PWD:$PATH

  $ cat > foo.t <<EOF
  >   $ echo "foo from foo.t"
  > EOF

  $ dune runtest --auto-promote
  bash.ml
  File "foo.t", line 1, characters 0-0:
  Error: Files _build/default/foo.t and _build/default/foo.t.corrected differ.
  Promoting _build/default/foo.t.corrected to foo.t.
  [1]

  $ dune runtest -f
  bash.ml
  $ cat foo.t
    $ echo "foo from foo.t"
    ***** UNREACHABLE *****

Clean-up for the following test cases

  $ rm -rf sh bash
  $ dune clean
  $ ls
  bash.ml
  custom_shell.ml
  dune
  dune-project
  foo.t
  sh.ml

Check that shell = %{bin:bash} uses bash

  $ cat > dune <<EOF
  > (cram (shell %{bin:bash}))
  > EOF

  $ cat > foo.t <<EOF
  >   $ echo "foo from foo.t"
  > 
  > EOF
  $ echo '  $ [ ! -z $BASH ] && echo "shell = bash" || echo "shell <> bash"' >> foo.t


  $ dune runtest --auto-promote
  File "foo.t", line 1, characters 0-0:
  Error: Files _build/default/foo.t and _build/default/foo.t.corrected differ.
  Promoting _build/default/foo.t.corrected to foo.t.
  [1]

  $ dune runtest -f
  $ cat foo.t
    $ echo "foo from foo.t"
    foo from foo.t
  
    $ [ ! -z $BASH ] && echo "shell = bash" || echo "shell <> bash"
    shell = bash

Check that shell = %{dep:./custom_shell.exe} uses executable compiled
from custom_shell.ml

  $ cat > dune <<EOF
  > (cram (shell %{dep:./custom_shell.exe}))
  > (executables (names custom_shell))
  > EOF

  $ dune build && ls
  _build
  bash.ml
  custom_shell.ml
  dune
  dune-project
  foo.t
  sh.ml

  $ cat > foo.t <<EOF
  >   $ echo "foo from foo.t"
  > EOF

  $ dune runtest --auto-promote
  custom_shell.ml
  File "foo.t", line 1, characters 0-0:
  Error: Files _build/default/foo.t and _build/default/foo.t.corrected differ.
  Promoting _build/default/foo.t.corrected to foo.t.
  [1]

  $ dune runtest -f
  custom_shell.ml
  $ cat foo.t
    $ echo "foo from foo.t"
    ***** UNREACHABLE *****

  $ ls
  _build
  bash.ml
  custom_shell.ml
  dune
  dune-project
  foo.t
  sh.ml

Check that shell = ./custom_shell.exe uses executable compiled
from custom_shell.ml

  $ cat > dune <<EOF
  > (cram (shell ./custom_shell.exe))
  > (executables (names custom_shell))
  > EOF

  $ dune build && ls
  _build
  bash.ml
  custom_shell.ml
  dune
  dune-project
  foo.t
  sh.ml

  $ cat > foo.t <<EOF
  >   $ echo "foo from foo.t"
  > EOF

  $ dune runtest --auto-promote
  custom_shell.ml
  File "foo.t", line 1, characters 0-0:
  Error: Files _build/default/foo.t and _build/default/foo.t.corrected differ.
  Promoting _build/default/foo.t.corrected to foo.t.
  [1]

  $ dune runtest -f
  custom_shell.ml
  $ cat foo.t
    $ echo "foo from foo.t"
    ***** UNREACHABLE *****

  $ ls
  _build
  bash.ml
  custom_shell.ml
  dune
  dune-project
  foo.t
  sh.ml
