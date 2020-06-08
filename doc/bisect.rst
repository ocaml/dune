*************************
Code coverage with bisect
*************************

In this section, we will explain how to set up code coverage with bisect_ppx_ so
that you can enable and disable coverage via ``dune-workspace`` files. This
setup avoids creating a hard dependency on ``bisect_ppx`` in your project.

Specifying what to bisect
=========================

First we must include ``(using bisect_ppx 1.0)`` in our ``dune-project`` file,
like so:

.. code:: scheme

          (lang dune 2.7)
          (using bisect_ppx 1.0)

Then, we should use the ``(bisect_ppx)`` field. The dune file may look like
this:

.. code:: scheme

          (library
           (name foo)
           (modules foo)
           (bisect_ppx))

          (executable
           (name test)
           (modules test)
           (libraries foo))

The ``(bisect_ppx)`` field can be specified in library and executable stanzas.
Libraries/executables that do not use ``(bisect_ppx)`` will not be instrumented
for code coverage.

Enabling/disabling code coverage
================================

By default, ``bisect_ppx`` is not compiled and linked with the program when
using ``(bisect_ppx)``. To enable code coverage, we can set the
``bisect_enabled``  flag in a ``dune-workspace`` file. For example,
``dune-workspace.dev`` may look like:

.. code:: scheme

          (lang dune 2.7)
          (context (default (bisect_enabled true)))

Then, to build the project with code coverage, we can run:

.. code:: bash

          $ dune exec ./test.exe --workspace dune-workspace.dev

We can also define different contexts in the ``dune-workspace`` file as follows:

.. code:: scheme

          (lang dune 2.7)
          (context default)
          (context (default (name coverage) (bisect_enabled true)))

Running the following will enable coverage:

.. code:: bash

          $ dune exec ./test.exe --context coverage

.. _bisect_ppx: https://github.com/aantron/bisect_ppx
