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

.. _locks:

Locks
-----

Given two rules that are independent, dune will assume that there
associated action can be run concurrently. Two rules are considered
independent if none of them depend on the other, either directly or
through a chain of dependencies. This basic assumption allows to
parallelize the build.

However, it is sometimes the case that two independent rules cannot be
executed concurrently. For instance this can happen for more
complicated tests. In order to prevent dune from running the
actions at the same time, you can specify that both actions take the
same lock:

.. code:: scheme

    (alias
     (name   runtest)
     (deps   foo)
     (locks  m)
     (action (run test.exe %{deps})))

    (alias
     (name   runtest)
     (deps   bar)
     (locks  m)
     (action (run test.exe %{deps})))

Dune will make sure that the executions of ``test.exe foo`` and
``test.exe bar`` are serialized.

Although they don't live in the filesystem, lock names are interpreted as file
names. So for instance ``(with-lock m ...)`` in ``src/dune`` and ``(with-lock
../src/m)`` in ``test/dune`` refer to the same lock.

Note also that locks are per build context. So if your workspace has two build
contexts setup, the same rule might still be executed concurrently between the
two build contexts. If you want a lock that is global to all build contexts,
simply use an absolute filename:

.. code:: scheme

    (alias
     (name   runtest)
     (deps   foo)
     (locks  /tcp-port/1042)
     (action (run test.exe %{deps})))
