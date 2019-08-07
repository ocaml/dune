************
Common items
************

.. _library-dependencies:

Library dependencies
--------------------

Dependencies on libraries are specified using ``(libraries ...)`` fields in
``library`` and ``executables`` stanzas.

For libraries defined in the current scope, you can use either the real name or
the public name. For libraries that are part of the installed world, or for
libraries that are part of the current workspace but in another scope, you need
to use the public name. For instance: ``(libraries base re)``.

When resolving libraries, libraries that are part of the workspace are always
preferred to ones that are part of the installed world.

.. _alternative-deps:

Alternative dependencies
~~~~~~~~~~~~~~~~~~~~~~~~

In addition to direct dependencies you can specify alternative dependencies.
This is described in the :ref:`Alternative dependencies <alternative-deps>`
section

It is sometimes the case that one wants to not depend on a specific library, but
instead on whatever is already installed. For instance to use a different
backend depending on the target.

Dune allows this by using a ``(select ... from ...)`` form inside the list
of library dependencies.

Select forms are specified as follows:

.. code:: scheme

    (select <target-filename> from
     (<literals> -> <filename>)
     (<literals> -> <filename>)
     ...)

``<literals>`` are lists of literals, where each literal is one of:

- ``<library-name>``, which will evaluate to true if ``<library-name>`` is
  available, either in the workspace or in the installed world
- ``!<library-name>``, which will evaluate to true if ``<library-name>`` is not
  available in the workspace or in the installed world

When evaluating a select form, dune will create ``<target-filename>`` by
copying the file given by the first ``(<literals> -> <filename>)`` case where
all the literals evaluate to true. It is an error if none of the clauses are
selectable. You can add a fallback by adding a clause of the form ``(->
<file>)`` at the end of the list.

.. _ocaml-flags:

OCaml flags
-----------

In ``library``, ``executable``, ``executables`` and ``env`` stanzas,
you can specify OCaml compilation flags using the following fields:

- ``(flags <flags>)`` to specify flags passed to both ``ocamlc`` and
  ``ocamlopt``
- ``(ocamlc_flags <flags>)`` to specify flags passed to ``ocamlc`` only
- ``(ocamlopt_flags <flags>)`` to specify flags passed to ``ocamlopt`` only

For all these fields, ``<flags>`` is specified in the :ref:`ordered-set-language`.
These fields all support ``(:include ...)`` forms.

The default value for ``(flags ...)`` is taken from the environment,
as a result it is recommended to write ``(flags ...)`` fields as
follows:

.. code:: scheme

    (flags (:standard <my options>))
