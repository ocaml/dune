@lint
=====

This alias runs the linters configured by :ref:`lint field <lint-field>`.

For example, ``dune build @lint`` runs linters in the current directory and its
subdirectories. If a PPX linter reports corrections, Dune displays them as
diffs that can be applied with ``dune promote``.
