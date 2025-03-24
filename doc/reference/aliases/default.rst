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
