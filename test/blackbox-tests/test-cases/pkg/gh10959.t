Repro `dune exec --watch` crash with pkg management

  $ . ./helpers.sh

  $ cat >dune-project <<EOF
  > (lang dune 3.13)
  > (pin
  >  (url file://$PWD/_multiple)
  >  (package (name foo))
  >  (package (name bar)))
  > (package
  >  (name main)
  >  (depends foo bar))
  > EOF

  $ cat >dune <<EOF
  > (executable
  >  (name x))
  > EOF
  $ cat >x.ml <<EOF
  > let () = print_endline "x"
  > EOF

  $ mkdir dune.lock
  $ cat >dune.lock/lock.dune <<EOF
  > (lang package 0.1)
  > (repositories
  >  (complete true))
  > EOF

  $ cat >dune.lock/test.pkg <<EOF
  > (version 0.0.1)
  > (build
  >  (system "echo building test"))
  > EOF

  $ dune exec -w ./x.exe 2>&1 | grep -io "I must not crash"
  I must not crash

