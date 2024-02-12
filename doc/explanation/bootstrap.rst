How Dune Uses Dune to Build Dune
================================

Dune's build system is itself Dune. This works thanks to a bootstrap process.
This document explains how this works.

``boot/bootstrap.ml``
---------------------

``boot/bootstrap.ml`` is an OCaml script (it is interpreted, not compiled) that
is a mini-build system tailored to Dune itself. It computes dependencies
between the various modules by calling ``ocamldep``, and it will generate build
and link commands. It knows how to execute these commands in parallel. It does
not read any ``dune`` file. However, the project structure and its system
dependencies are encoded in ``boot/libs.ml``.

This step produces ``_boot/dune.exe``.

Completing the Opam Installation
--------------------------------

``_boot/dune.exe`` is the bootstrap Dune. Since it has been built from
the Dune sources, it will act like Dune: it can read ``dune`` files, etc.

This is actually the ``dune`` executable that will get installed. But Opam does
not know about this: it expects a ``dune.install`` file that explains what
files to install.

The next command run by the Opam instruction is the following:

.. code:: console

   $ ./_boot/dune.exe build dune.install --release --profile dune-bootstrap

By using the ``dune-bootstrap`` :term:`build profile`, it not run a full build,
but only copy ``_boot/dune.exe`` to its install location, and generate
``dune.install``.

``make dev``: Everything Else
-----------------------------

The above describes how Dune itself is built through Opam, but that's not all
there is it to it: the Dune repository contains other libraries that need to be
built, the bootstrap Dune did not generate files useful for editor integration,
and it can not do incremental builds.

So the main ``Makefile`` has a ``make dev`` target that will run
``_boot/dune.exe build @install``: this will rebuild the project using Dune
itself.

As a special rule, this build will regenerate ``boot/libs.ml`` using the
locations of the internal libraries used to build Dune.
