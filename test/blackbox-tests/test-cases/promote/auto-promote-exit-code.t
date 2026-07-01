Regression test for https://github.com/ocaml/dune/issues/785.

`dune build --auto-promote` applies the promotion but currently still exits
with a non-zero code, forcing callers to wrap the command in `|| true`. This
test captures that behaviour; once #785 is fixed the build should exit 0 once
the promotion has been applied (a fixpoint is reached).

  $ make_dune_project 3.25

  $ cat > dune <<EOF
  > (rule
  >  (with-stdout-to x.gen (echo "toto")))
  > (rule
  >  (alias blah)
  >  (action (diff x x.gen)))
  > EOF

  $ printf titi > x

The diff is shown and the file is promoted, but the build still exits with
code 1 -- this is the bug tracked by #785:

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
  [1]
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
