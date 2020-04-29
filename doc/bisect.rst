*************************
Code coverage with bisect
*************************

Normally, preprocessors (specified in ``dune`` files) are constant across all
builds. However, it usually doesn't make sense to analyze code coverage on all
builds, especially if you aren't running tests every time.

In this section, we will explain how to set up code coverage with bisect_ppx_ so
that you can enable and disable coverage via ``dune-workspace`` files.

Specifying what to bisect
=========================

To measure coverage for every build, we can include ``bisect_ppx`` in the
``preprocess`` field for each relevant library and executable. For example, the
dune file would include stanza(s) like this:

.. code:: scheme

          (library
           (name foo)
           (preprocess (pps bisect_ppx)))

This aligns with the standard usage of ppx libraries.

However, if we would like to control which builds measure coverage, first we
must include ``(using bisect_ppx 1.0)`` in our ``dune-project`` file, like so:

.. code:: scheme

          (lang dune 2.6)
          (using bisect_ppx 1.0)

Then, instead of including ``bisect_ppx`` in ``preprocess``, we should use the
``(bisect_ppx)`` field. The dune file may look like this:

.. code:: scheme

          (library
           (name foo)
           (modules foo)
           (bisect_ppx))

          (executable
           (name test)
           (modules test))

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

          (lang dune 2.6)
          (context (default (bisect_enabled true)))

Then, to build the project with code coverage, we can run:

.. code:: bash

          $ dune exec ./test.exe --workspace dune-workspace.dev

.. _bisect_ppx: https://github.com/aantron/bisect_ppx
