A cram stanza may not match any tests:

  $ make_dune_project 3.21

This stanza does not apply to anything:

  $ cat >dune <<EOF
  > (cram
  >  (applies_to foobar.t)
  >  (deps x))
  > EOF

The user likely meant to write foobar rather than foobar.t

  $ cat >foobar.t <<EOF
  >   $ echo foo
  > EOF

Demonstrate that the stanza doesn't apply to anything because this test
shouldn't even run if it depends on a file x.

  $ dune runtest foobar.t
  File "foobar.t", line 1, characters 0-0:
  --- foobar.t
  +++ foobar.t.corrected
  @@ -1 +1,2 @@
     $ echo foo
  +  foo
  [1]
