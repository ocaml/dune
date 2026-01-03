The interaction and order of overriding [DUNE_ROOT] and [--root].

Feature request https://github.com/ocaml/dune/issues/12399

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

When the root is [.], specifying it is redundant:

  $ dune runtest --force
  root
  subdir1
  subdir2

  $ dune runtest --force --root=.
  root
  subdir1
  subdir2

  $ DUNE_ROOT=. dune runtest --force
  root
  subdir1
  subdir2

When the root is a different directory, honor it:

  $ dune runtest --force --root=./subdir1
  subdir1

  $ DUNE_ROOT=./subdir1 dune runtest --force
  subdir1

  $ cd subdir1 && dune runtest --force && cd ..
  subdir1

  $ cd subdir1 && dune runtest --force --root=. && cd ..
  subdir1

  $ cd subdir1 && DUNE_ROOT=. dune runtest --force && cd ..
  subdir1

  $ cd subdir1 && dune runtest --force --root=.. && cd ..
  root
  subdir1
  subdir2

  $ cd subdir1 && DUNE_ROOT=.. dune runtest --force && cd ..
  root
  subdir1
  subdir2

When both [--root] and [DUNE_ROOT] are specified, [--root] has priority:

  $ DUNE_ROOT=./subdir1 dune runtest --force --root=./subdir2
  subdir2
