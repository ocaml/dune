Tests
=====

The :file:`test/` directory contains all the tests for Dune itself. Additionally,
the tests for our :doc:`libraries` are stored in :file:`otherlibs/` next to the
library itself.

We have 3 kind of tests:

- Unit tests, in :file:`test/unit-tests` (we have very few of these, usually
  prefering other kinds)
- Expect tests, in :file:`test/expect-tests` (using ``ppx_expect``)
- :doc:`Cram tests </reference/cram>`, in :file:`test/blackbox-tests/`. This is
  our preferred way of testing.

The actual Cram tests are in :file:`test/expect-tests/test-cases`. There is a
mix of file tests and directory tests. For regression tests, the pattern
``githubNUMBER.t`` is used.

The ``dune`` file at :file:`test/expect-tests/test-cases/dune` sets up some
metadata for the tests. For example, if a test has an external dependency like
``strace``, a dependency on ``%{bin:strace}`` will prevent the test from even
trying to start. Some tests are also disabled on some configurations using
``(enabled_if)``.

Finally, some programs available in the Cram tests are defined in
:file:`test/expect-tests/blackbox-tests/utils`. For example, we have `a
dune_cmd program
<https://github.com/ocaml/dune/blob/3.15.0/test/blackbox-tests/utils/dune_cmd.ml>`_
that contains reimplementations of common utilities like ``stat``, which do not
have the same output on the different systems we use to test Dune.

.. seealso:: :doc:`/hacking`
