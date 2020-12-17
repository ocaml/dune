  $ cat >dune-project <<EOF
  > (lang dune 2.7)
  > (allow_approximate_merlin true)
  > EOF

  $ dune build @all

  $ cat >dune-project <<EOF
  > (lang dune 2.8)
  > (allow_approximate_merlin true)
  > EOF

  $ dune build @all
  File "dune-project", line 2, characters 0-31:
  2 | (allow_approximate_merlin true)
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Warning: This field was deprecated in version 2.8 of the dune language. It is
  useless since the Merlin configurations are not ambiguous anymore.
