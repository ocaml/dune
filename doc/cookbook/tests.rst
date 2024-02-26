Defining Tests
==============

Write this in your ``dune`` file:

.. code:: dune

    (test (name my_test_program))

And run the tests with:

.. code:: console

  $ dune runtest

It will run the test program (the main module is ``my_test_program.ml``) and
error if it exits with a nonzero code.

In addition, if a ``my_test_program.expected`` file exists, it will be compared
to the standard output of the test program and the differences will be
displayed. It is possible to replace the ``.expected`` file with the last output
using:

.. code:: console

  $ dune promote
