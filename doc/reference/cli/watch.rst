``--watch`` - Watch Mode
========================

The ``dune build`` and ``dune runtest`` commands support a ``-w`` (or
``--watch``) flag. When it's passed, Dune will perform the action as usual and
then wait for file changes and rebuild (or rerun the tests). This feature
requires ``inotifywait`` or ``fswatch`` to be installed.
