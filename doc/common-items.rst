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

.. _diffing-and-promotion:

Diffing and promotion
---------------------

``(diff <file1> <file2>)`` is very similar to ``(run diff <file1>
<file2>)``. In particular it behaves in the same way:

- when ``<file1>`` and ``<file2>`` are equal, it doesn't nothing
- when they are not, the differences are shown and the action fails

However, it is different for the following reason:

- the exact command used to diff files can be configured via the
  ``--diff-command`` command line argument. Note that it is only
  called when the files are not byte equals

- by default, it will use ``patdiff`` if it is installed. ``patdiff``
  is a better diffing program. You can install it via opam with:

  .. code:: sh

     $ opam install patdiff

- on Windows, both ``(diff a b)`` and ``(diff? a b)`` normalize the end of
  lines before comparing the files

- since ``(diff a b)`` is a builtin action, dune knowns that ``a``
  and ``b`` are needed and so you don't need to specify them
  explicitly as dependencies

- you can use ``(diff? a b)`` after a command that might or might not
  produce ``b``. For cases where commands optionally produce a
  *corrected* file

- it allows promotion. See below

Note that ``(cmp a b)`` does no end of lines normalization and doesn't
print a diff when the files differ. ``cmp`` is meant to be used with
binary files.

Promotion
~~~~~~~~~

Whenever an action ``(diff <file1> <file2>)`` or ``(diff?  <file1>
<file2>)`` fails because the two files are different, dune allows
you to promote ``<file2>`` as ``<file1>`` if ``<file1>`` is a source
file and ``<file2>`` is a generated file.

More precisely, let's consider the following dune file:

.. code:: scheme

   (rule
    (with-stdout-to data.out (run ./test.exe)))

   (alias
    (name   runtest)
    (action (diff data.expected data.out)))

Where ``data.expected`` is a file committed in the source
repository. You can use the following workflow to update your test:

- update the code of your test
- run ``dune runtest``. The diff action will fail and a diff will
  be printed
- check the diff to make sure it is what you expect
- run ``dune promote``. This will copy the generated ``data.out``
  file to ``data.expected`` directly in the source tree

You can also use ``dune runtest --auto-promote`` which will
automatically do the promotion.

.. _ocaml-syntax:

OCaml syntax
============

If a ``dune`` file starts with ``(* -*- tuareg -*- *)``, then it is
interpreted as an OCaml script that generates the ``dune`` file as described
in the rest of this section. The code in the script will have access to a
`Jbuild_plugin
<https://github.com/ocaml/dune/blob/master/plugin/jbuild_plugin.mli>`__
module containing details about the build context it is executed in.

The OCaml syntax gives you an escape hatch for when the S-expression
syntax is not enough. It is not clear whether the OCaml syntax will be
supported in the long term as it doesn't work well with incremental
builds. It is possible that it will be replaced by just an ``include``
stanza where one can include a generated file.

Consequently **you must not** build complex systems based on it.
