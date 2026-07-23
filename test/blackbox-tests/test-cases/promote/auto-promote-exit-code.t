Regression test for https://github.com/ocaml/dune/issues/785.

`dune build --auto-promote` applies the promotion and exits with code 0,
instead of forcing callers to wrap the command in `|| true`.

  $ make_dune_project 3.25

  $ cat > dune <<EOF
  > (rule
  >  (with-stdout-to x.gen (echo "toto")))
  > (rule
  >  (alias blah)
  >  (action (diff x x.gen)))
  > EOF

  $ printf titi > x

The diff is shown and the file is promoted, and the build exits 0:

  $ dune build @blah --auto-promote
  File "x", line 1, characters 0-0:
  --- x
  +++ x.gen
  @@ -1 +1 @@
  -titi
  \ No newline at end of file
  +toto
  \ No newline at end of file
  Promoting _build/default/x.gen to x.
  $ cat x
  toto

Re-running reaches a fixpoint and is a clean no-op:

  $ dune build @blah --auto-promote
  $ cat x
  toto

A diff that cannot be promoted back into the source tree (both operands are
generated build artifacts) is a genuine failure that --auto-promote must not
mask:

  $ cat > dune <<EOF
  > (rule
  >  (with-stdout-to a (echo aaa)))
  > (rule
  >  (with-stdout-to b (echo bbb)))
  > (rule
  >  (alias notpromotable)
  >  (action (diff a b)))
  > EOF

  $ dune build @notpromotable --auto-promote
  File "a", line 1, characters 0-0:
  --- a
  +++ b
  @@ -1 +1 @@
  -aaa
  \ No newline at end of file
  +bbb
  \ No newline at end of file
  [1]

Result-producing commands must not treat an auto-promoted diff as success for
the current run. The executable is not linked until the command is re-run after
promotion reaches a fixpoint.

  $ cat > dune <<EOF
  > (executable
  >  (name foo)
  >  (link_deps (alias blah)))
  > (rule
  >  (with-stdout-to x.gen (echo "toto")))
  > (rule
  >  (alias blah)
  >  (action (diff x x.gen)))
  > EOF

  $ printf titi > x
  $ cat > foo.ml <<EOF
  > let () = print_endline "foo"
  > EOF

  $ dune exec --auto-promote ./foo.exe
  File "x", line 1, characters 0-0:
  --- x
  +++ x.gen
  @@ -1 +1 @@
  -titi
  \ No newline at end of file
  +toto
  \ No newline at end of file
  Promoting _build/default/x.gen to x.
  [1]

  $ dune exec --auto-promote ./foo.exe
  foo
