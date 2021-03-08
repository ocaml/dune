The vendored project does not trigger a third warning

When the root project is on dune lang <= 2.7 it does not raise a warning
However the non-vendored sub-dir on dune lang >= 2.8 does raise a warning
  $ printf "(lang dune 2.7)\n (allow_approximate_merlin)" > dune-project
  $ dune build @check
  File "notvendor/dune-project", line 2, characters 0-26:
  2 | (allow_approximate_merlin)
      ^^^^^^^^^^^^^^^^^^^^^^^^^^
  Warning: This field was deprecated in version 2.8 of the dune language. It is
  useless since the Merlin configurations are not ambiguous anymore.
