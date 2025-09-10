The interaction and order of overriding [DUNE_ROOT] and [--root].

Feature request github.com/ocaml/dune/issues/12399

Create a [dune-project] and [dune] at the root and in 2 subdirectories:

  $ cat >dune-project <<EOF
  > (lang dune 3.21)
  > EOF
  $ cat >dune <<EOF
  > (rule
  >   (alias runtest)
  >   (action (echo "root\n")))
  > EOF

  $ mkdir subdir1
  $ cat >subdir1/dune-project <<EOF
  > (lang dune 3.21)
  > EOF
  $ cat >subdir1/dune <<EOF
  > (rule
  >   (alias runtest)
  >   (action (echo "subdir1\n")))
  > EOF

  $ mkdir subdir2
  $ cat >subdir2/dune-project <<EOF
  > (lang dune 3.21)
  > EOF
  $ cat >subdir2/dune <<EOF
  > (rule
  >   (alias runtest)
  >   (action (echo "subdir2\n")))
  > EOF

  $ alias cleanup='rm -rf `find . -type d -name _build`'

When the root is [.], specifying it is redundant:

  $ cleanup && dune runtest
  root
  subdir1
  subdir2

  $ cleanup && dune runtest --root=.
  root
  subdir1
  subdir2

  $ cleanup && DUNE_ROOT=. dune runtest
  root
  subdir1
  subdir2

When the root is a different directory, honor it:

  $ cleanup && dune runtest --root=./subdir1
  Entering directory 'subdir1'
  subdir1
  Leaving directory 'subdir1'

  $ cleanup && DUNE_ROOT=./subdir1 dune runtest
  Entering directory 'subdir1'
  subdir1
  Leaving directory 'subdir1'

  $ cleanup && cd subdir1 && dune runtest && cd ..
  subdir1

  $ cleanup && cd subdir1 && dune runtest --root=. && cd ..
  subdir1

  $ cleanup && cd subdir1 && DUNE_ROOT=. dune runtest && cd ..
  subdir1

  $ cleanup && cd subdir1 && dune runtest --root=.. && cd ..
  Entering directory '..'
  root
  subdir1
  subdir2
  Leaving directory '..'

  $ cleanup && cd subdir1 && DUNE_ROOT=.. dune runtest && cd ..
  Entering directory '..'
  root
  subdir1
  subdir2
  Leaving directory '..'

When both [--root] and [DUNE_ROOT] are specified, [--root] has priority:

  $ cleanup && DUNE_ROOT=./subdir1 dune runtest --root=./subdir2
  Entering directory 'subdir2'
  subdir2
  Leaving directory 'subdir2'
