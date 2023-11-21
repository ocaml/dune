We test how Dune handles dependencies on private packages.

  $ cat > dune-project <<EOF
  > (lang dune 3.13)
  > (package
  >  (name foo)
  >  (allow_empty)
  >  (private))
  > (package
  >  (name bar)
  >  (allow_empty)
  >  (depends foo))
  > EOF

  $ cat > dune <<EOF
  > (rule
  >  (alias foo)
  >  (deps
  >   (package foo))
  >  (action
  >   (echo "foo")))
  > (rule
  >  (alias bar)
  >  (deps
  >   (package bar))
  >  (action
  >   (echo "bar")))
  > EOF

You cannot use (package) to depend on a private package since it will not have an install
layout. Perhaps in the future a virtual one could be constructed. Dune should give an
error message indicating that.

  $ dune build @foo
  File "dune", line 4, characters 11-14:
  4 |   (package foo))
                 ^^^
  Error: Only non-private packages are accepted in (package) dependencies.
  [1]

The public package bar cannot depend on the private package foo and Dune should give an
error message indicating that.

For now Dune does not validate the depends field of (package) stanzas. So the following
builds when it shouldn't.

  $ dune build @bar 
  bar

