Check that Cram tests run without shell option specified

  $ echo '(lang dune 3.12)' > dune-project

  $ cat > dune <<EOF
  > (rule
  >  (target foo.cram.corrected)
  >  (mode (promote (until-clean)))
  >  (action (cram foo.cram)))
  > EOF

  $ cat > foo.cram <<EOF
  >   $ echo "foo from foo.cram"
  > EOF

  $ dune build
  $ cat foo.cram.corrected
    $ echo "foo from foo.cram"
    foo from foo.cram

Prepare custom and mock shells for following tests

  $ dune clean
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
  foo.cram
  sh
  sh.ml

  $ ./sh
  sh.ml
  $ ./bash
  bash.ml

Check that shell option unset uses `env sh`

  $ PATH=$PWD:$PATH
  $ cat > dune <<EOF
  > (rule
  >  (target foo.cram.corrected)
  >  (mode (promote (until-clean)))
  >  (action (cram foo.cram)))
  > EOF

  $ cat > foo.cram <<EOF
  >   $ echo "foo from foo.cram"
  > EOF

  $ dune build
  sh.ml
  $ cat foo.cram.corrected
    $ echo "foo from foo.cram"
    ***** UNREACHABLE *****

Check that shell = bash uses `env bash`

  $ PATH=$PWD:$PATH
  $ cat > dune <<EOF
  > (rule
  >  (target foo.cram.corrected)
  >  (mode (promote (until-clean)))
  >  (deps sh)
  >  (action (cram foo.cram (shell bash))))
  > EOF

  $ cat > foo.cram <<EOF
  >   $ echo "foo from foo.cram"
  > EOF

  $ dune build
  bash.ml
  $ cat foo.cram.corrected
    $ echo "foo from foo.cram"
    ***** UNREACHABLE *****

Check that shell = :system uses `env sh`

  $ PATH=$PWD:$PATH
  $ cat > dune <<EOF
  > (rule
  >  (target foo.cram.corrected)
  >  (mode (promote (until-clean)))
  >  (deps sh)
  >  (action (cram foo.cram (shell :system))))
  > EOF

  $ cat > foo.cram <<EOF
  >   $ echo "foo from foo.cram"
  > EOF

  $ dune build
  sh.ml
  $ cat foo.cram.corrected
    $ echo "foo from foo.cram"
    ***** UNREACHABLE *****

Clean-up for the following test cases

  $ rm -rf sh bash
  $ dune clean
  $ ls
  bash.ml
  custom_shell.ml
  dune
  dune-project
  foo.cram
  sh.ml

Check that shell = %{dep:./custom_shell.exe} uses executable compiled
from custom_shell.ml

  $ cat > dune <<EOF
  > (executables (names custom_shell))
  > (rule
  >  (target foo.cram.corrected)
  >  (mode (promote (until-clean)))
  >  (deps ./custom_shell.exe)
  >  (action (cram foo.cram (shell ./custom_shell.exe))))
  > EOF

  $ cat > foo.cram <<EOF
  >   $ echo "foo from foo.cram"
  > EOF

  $ dune build
  custom_shell.ml
  $ cat foo.cram.corrected
    $ echo "foo from foo.cram"
    ***** UNREACHABLE *****
