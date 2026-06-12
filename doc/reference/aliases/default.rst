@default
========

This alias corresponds to the default argument for ``dune build``: ``dune
build`` is equivalent to ``dune build @@default`` (``@@`` indicates a
:doc:`non-recursive alias <../aliases>`). Similarly, ``dune build dir`` is
equivalent to ``dune build @@dir/default``.

When a directory doesn't explicitly define what the ``default`` alias means via
an :doc:`/reference/dune/alias` stanza, the following implicit definition is
assumed:

.. code:: dune

   (alias
    (name default)
    (deps (alias_rec all)))

But if such a stanza is present in the ``dune`` file in a directory, it will be
used instead. For example, if the following is present in ``tests/dune``,
``dune build tests`` will run tests there:

.. code:: dune

   (alias
    (name default)
    (deps (alias_rec runtest)))

Interaction With Tests
~~~~~~~~~~~~~~~~~~~~~~

The default alias is distinct from the :doc:`runtest` alias. Plain
``dune build`` builds ``@@default``. It doesn't run actions attached only to
``runtest`` unless they are requested explicitly, for example with
``dune runtest`` or ``dune build @runtest``.

However, the implicit definition of ``default`` depends recursively on
:doc:`all`, and ``all`` includes file targets produced by tests, executables,
libraries, and rules. As a result, plain ``dune build`` can still build test
executables or generated test artifacts in subdirectories.

To keep a tests directory out of plain ``dune build``, override ``default`` in a
parent directory so that it only depends on the subtrees that should be part of
the ordinary build. For example, a project with ``bin/``, ``lib/``, and
``tests/`` can put this in the top-level ``dune`` file:

.. code:: dune

   (alias
    (name default)
    (deps
     (alias_rec bin/all)
     (alias_rec lib/all)))

Overriding ``default`` inside ``tests/dune`` would only affect commands such as
``dune build tests`` or ``dune build @@tests/default``; it would not change the
implicit recursive dependency used by the parent directory's ``default`` alias.
