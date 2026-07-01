@all
====

This alias corresponds to every known file target in a directory. It is not a
hard-coded list of stanza kinds: it includes targets introduced by user rules
as well as targets generated for libraries, executables, tests, and other
stanzas
in that directory.

For example, ``@all`` can include executable targets such as ``foo.exe`` or
``foo.bc``, library artifacts, and generated files from ``rule`` stanzas.
Some stanzas may also attach additional dependencies to ``@all`` explicitly;
for example, ``melange.emit`` attaches its emission targets to ``@all``.

The :doc:`default` alias depends recursively on ``all`` unless it is
overridden, so plain ``dune build`` usually builds ``@all`` in the workspace
subtree.

Since version 2.0 of the dune language, JS targets of executables are no longer
included in the ``all`` alias by default. To get back the old behavior of
including the JS targets in ``all``, one can add the ``js`` target to the
executable's ``modes`` field.
