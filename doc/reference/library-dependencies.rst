Library Dependencies
====================

Library dependencies are specified using ``(libraries ...)`` fields in
``library`` and ``executables`` stanzas.

For libraries defined in the current scope, you can either use the real name or
the public name. For libraries that are part of the :term:`installed world`, or
for libraries that are part of the current workspace but in another scope, you
need to use the public name. For instance: ``(libraries base re)``.

When resolving libraries, ones that are part of the workspace are always
preferred to ones that are part of the :term:`installed world`.

Alternative Dependencies
------------------------

Sometimes, one doesn't want to depend on a specific library but rather
on whatever is already installed, e.g., to use a different
backend, depending on the target.

Dune allows this by using a ``(select ... from ...)`` form inside the list
of library dependencies.

Select forms are specified as follows:

.. code:: dune

    (select <target-filename> from
     (<literals> -> <filename>)
     (<literals> -> <filename>)
     ...)

``<literals>`` are lists of literals, where each literal is one of:

- ``<library-name>``, which will evaluate to true if ``<library-name>`` is
  available, either in the workspace or in the :term:`installed world`
- ``!<library-name>``, which will evaluate to true if ``<library-name>`` is not
  available in the workspace or in the :term:`installed world`

When evaluating a select form, Dune will create ``<target-filename>`` by
copying the file given by the first ``(<literals> -> <filename>)`` case where
all the literals evaluate to true. It is an error if none of the clauses are
selectable. You can add a fallback by adding a clause of the form ``(->
<file>)`` at the end of the list.

Re-Exported Dependencies
------------------------

A dependency ``foo`` may be marked as always *re-exported* using the
following syntax:

.. code:: dune

   (re_export foo)

For instance:

.. code:: dune

   (library
    (name bar)
    (libraries (re_export foo)))

This states that this library explicitly re-exports the interface of
``foo``. Concretely, when something depends on ``bar``, it will also
be able to see ``foo`` independently of whether :doc:`implicit
transitive dependencies<dune-project/implicit_transitive_deps>` are
allowed or not. When they are allowed, which is the default, all transitive
dependencies are visible, whether they are marked as re-exported or not.
