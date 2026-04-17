@opam
=====

Building this alias generates ``*.opam`` files for projects that use
:doc:`/reference/dune-project/generate_opam_files`.

This is the dedicated alias for opam file generation. The
:doc:`install <install>` and :doc:`runtest <runtest>` aliases depend on it, so
building either of those will also refresh the generated ``*.opam`` files.

With ``(lang dune 3.23)`` or newer, when a checked-in ``*.opam`` file differs
from the generated one, Dune records the change as a promotion. Apply it with
``dune promote``.

With older Dune language versions, building ``@opam`` promotes the generated
``*.opam`` files to the source tree automatically.
