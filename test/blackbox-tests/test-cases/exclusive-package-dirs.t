Packages can be marked as existing in a particular directory.

First, demonstrate that packages cannot try to occupy the same directory:

  $ cat >dune-project <<EOF
  > (lang dune 3.21)
  > (package
  >  (name foo)
  >  (dir foo))
  > (package
  >  (name bar)
  >  (dir foo))
  > EOF

  $ dune build
  File "dune-project", line 7, characters 6-9:
  7 |  (dir foo))
            ^^^
  Error: package foo is already defined in "foo"
  [1]

  $ cat >dune-project <<EOF
  > (lang dune 3.21)
  > (package
  >  (name foo)
  >  (dir foo))
  > (package
  >  (name bar))
  > EOF

  $ mkdir foo
  $ cat >foo/dune <<EOF
  > (rule (with-stdout-to x.txt (echo foo bar)))
  > EOF

  $ dune build foo/x.txt

This should error because foo/x.txt is only visible to package foo

  $ dune build --only-packages bar foo/x.txt
  Error: Don't know how to build foo/x.txt
  [1]

We can try to define a rule that belongs to "foo" elsewhere, but it should not
work:

  $ mkdir bar
  $ cat >bar/dune <<EOF
  > (executable
  >  (public_name bar)
  >  (package foo))
  > EOF
  $ touch bar/bar.ml

  $ dune build @check
  File "bar/dune", line 3, characters 10-13:
  3 |  (package foo))
                ^^^
  Error: Package foo may not be defined here
  That package must exist in foo
  [1]

  $ rm -rf bar/

It should also be impossible to add non foo stanzas to foo/

  $ cat >foo/dune <<EOF
  > (executable
  >  (public_name foo)
  >  (package bar))
  > EOF
  $ touch foo/foo.ml

  $ dune build @check
  File "foo/dune", line 3, characters 10-13:
  3 |  (package bar))
                ^^^
  Error: Package bar may not be defined here
  The only package that can be defined in this directory is foo because the
  directory of this stanza is exclusive to this package
  [1]
