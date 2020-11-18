The vendored project does not trigger a third warning.

  $ dune build @check
  File "dune-project", line 2, characters 0-26:
  2 | (allow_approximate_merlin)
      ^^^^^^^^^^^^^^^^^^^^^^^^^^
  Warning: This field was deprecated in version 2.8 of the dune language. It is
  useless since the Merlin configurations are not ambiguous anymore.
  File "notvendor/dune-project", line 2, characters 0-26:
  2 | (allow_approximate_merlin)
      ^^^^^^^^^^^^^^^^^^^^^^^^^^
  Warning: This field was deprecated in version 2.8 of the dune language. It is
  useless since the Merlin configurations are not ambiguous anymore.
