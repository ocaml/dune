.. _running-tests:

runtest - Running Tests
=======================

There are two ways to run tests:

-  ``dune build @runtest``
-  ``dune test`` (or the more explicit ``dune runtest``)

The two commands are equivalent, and they will run all the tests defined in the
current directory and its children directories recursively. You can also run the tests in a
specific sub-directory and its children by using:

-  ``dune build @foo/bar/runtest``
-  ``dune test foo/bar`` (or ``dune runtest foo/bar``)
