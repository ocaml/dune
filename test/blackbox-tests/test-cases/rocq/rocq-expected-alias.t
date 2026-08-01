Testing that the diff rules are attached to the @runtest alias of the right
directory. This used to not be the case, and these rules would be attached
to the @runtest alias of the directory defining the rocq.theory stanza, even
for files living in its sub-directories.

  $ make_rocq_project 3.22 0.12

  $ cat > dune <<EOF
  > (include_subdirs qualified)
  > (rocq.theory
  >  (name a))
  > EOF

  $ mkdir sub
  $ cat > sub/foo.v <<EOF
  > Locate bool.
  > EOF
  $ touch sub/foo.expected

This has always worked:

  $ dune runtest
  File "sub/foo.expected", line 1, characters 0-0:
  --- sub/foo.expected
  +++ sub/foo.output
  @@ -0,0 +1 @@
  +Inductive Corelib.Init.Datatypes.bool
  [1]

But this used to fail:

  $ dune runtest sub
  File "sub/foo.expected", line 1, characters 0-0:
  --- sub/foo.expected
  +++ sub/foo.output
  @@ -0,0 +1 @@
  +Inductive Corelib.Init.Datatypes.bool
  [1]


