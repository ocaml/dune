################
 Profiling Dune
################

..
   TODO(diataxis)
   - reference: the CLI
   - howto: profiling a dune build

If ``--trace-file FILE`` is passed, Dune will write detailed data about
internal operations, such as the timing of commands that Dune runs.

The format is compatible with `Catapult trace-viewer`_. In particular,
these files can be loaded into Chromium's ``chrome://tracing``. Note
that the exact format is subject to change between versions.

.. _catapult trace-viewer: https://github.com/catapult-project/catapult/blob/master/tracing/README.md
