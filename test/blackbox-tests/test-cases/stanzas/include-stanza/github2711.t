Dirs stanzas from included files are evaluated.

  $ make_dune_project 2.7
  $ cat >dune.inc <<EOF
  > (dirs foo)
  > (rule (with-stdout-to foo (echo bar)))
  > EOF
  $ cat >dune <<EOF
  > (include dune.inc)
  > EOF
  $ dune build ./foo
  $ cat _build/default/foo
  bar

Multiple dirs stanzas are composed in order across dune and include files.

  $ mkdir composed
  $ cd composed
  $ make_dune_project 3.22
  $ mkdir keep foo bar
  $ cat >keep/dune <<EOF
  > (rule (with-stdout-to ok (echo keep)))
  > EOF
  $ cat >foo/dune <<EOF
  > (rule (with-stdout-to nope (echo foo)))
  > EOF
  $ cat >bar/dune <<EOF
  > (rule (with-stdout-to nope (echo bar)))
  > EOF
  $ cat >dune.inc <<EOF
  > (dirs :standard \ foo)
  > EOF
  $ cat >dune <<EOF
  > (dirs :standard \ bar)
  > (include dune.inc)
  > EOF
  $ dune build ./keep/ok
  $ cat _build/default/keep/ok
  keep
  $ dune build ./foo/nope
  Error: Don't know how to build ./foo/nope
  [1]
  $ dune build ./bar/nope
  Error: Don't know how to build ./bar/nope
  [1]
  $ cd ..

Later dirs stanzas can replace previous results when they don't use :standard.

  $ mkdir override
  $ cd override
  $ make_dune_project 3.22
  $ mkdir keep other
  $ cat >keep/dune <<EOF
  > (rule (with-stdout-to ok (echo keep)))
  > EOF
  $ cat >other/dune <<EOF
  > (rule (with-stdout-to nope (echo other)))
  > EOF
  $ cat >dune.inc <<EOF
  > (dirs keep)
  > EOF
  $ cat >dune <<EOF
  > (dirs :standard \ keep)
  > (include dune.inc)
  > EOF
  $ dune build ./keep/ok
  $ cat _build/default/keep/ok
  keep
  $ dune build ./other/nope
  Error: Don't know how to build ./other/nope
  [1]
  $ cd ..

Before version 3.22, multiple dirs stanzas are rejected.

  $ mkdir pre-3-22
  $ cd pre-3-22
  $ make_dune_project 3.21
  $ cat >dune.inc <<EOF
  > (dirs foo)
  > EOF
  $ cat >dune <<EOF
  > (dirs bar)
  > (include dune.inc)
  > EOF
  $ dune build
  File "dune.inc", line 1, characters 6-9:
  1 | (dirs foo)
            ^^^
  Error: may not set the "dirs" stanza more than once
  [1]
  $ cd ..
