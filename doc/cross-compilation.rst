.. _cross-compilation:

*****************
Cross-Compilation
*****************

Dune allows for cross-compilation by defining build contexts with multiple
targets. Targets are specified by adding a ``targets`` field to the build
context definition.

``targets`` takes a list of target name. It can be either:

- ``native``, the native tools that can build binaries to run on the machine
  doing the build

- the name of an alternative toolchain

Note that at the moment, there is no official support for cross-compilation in
OCaml. Dune supports the `opam-cross-<x>` repositories from the `OCaml-cross
organization on GitHub <https://github.com/ocaml-cross/>`_, such as:

- `opam-cross-windows <https://github.com/ocaml-cross/opam-cross-windows>`_
- `opam-cross-android <https://github.com/ocaml-cross/opam-cross-android>`_
- `opam-cross-ios <https://github.com/ocaml-cross/opam-cross-ios>`_

In particular:

- to build Windows binaries using opam-cross-windows, write ``windows`` in the
  list of targets
- to build Android binaries using opam-cross-android, write ``android`` in the
  list of targets
- to build IOS binaries using opam-cross-ios, write ``ios`` in the list of
  targets

For example, the following workspace file defines three different targets for
the ``default`` build context:

.. code:: scheme

    (context (default (targets native windows android)))

This configuration defines three build contexts:

- ``default``
- ``default.windows``
- ``default.android``

Note that the ``native`` target is always implicitly added when not present;
however, ``dune build @install`` will skip this context, i.e., ``default`` will
only be used for building executables needed by the other contexts.

With such a setup, calling ``dune build @install`` will build all the packages
three times.

Note that instead of writing a ``dune-workspace`` file, you can also use the
``-x`` command line option. Passing ``-x foo`` to ``dune`` without having a
``dune-workspace`` file is the same as writing the following ``dune-workspace``
file:

.. code:: scheme

   (context (default (targets foo)))

If you have a ``dune-workspace`` and pass a ``-x foo`` option, ``foo`` will be
added as target of all context stanzas.

How Does it Work?
=================

In such a setup, binaries that need to be built and executed in the
``default.windows`` or ``default.android`` contexts as part of the build will
no longer be executed. Instead, all the binaries that will be executed come
from the ``default`` context. One consequence of this is that all preprocessing
(PPX or otherwise) will be done using binaries built in the ``default``
context.

To clarify this with an example, let's assume that you have the following
``src/dune`` file:

.. code:: scheme

    (executable (name foo))
    (rule (with-stdout-to blah (run ./foo.exe)))

When building ``_build/default/src/blah``, dune will resolve ``./foo.exe`` to
``_build/default/src/foo.exe`` as expected. However, for
``_build/default.windows/src/blah`` dune will resolve ``./foo.exe`` to
``_build/default/src/foo.exe``

Assuming that the right packages are installed or that your workspace has no
external dependencies, Dune will be able to cross-compile a given package
without doing anything special.

Some packages might still have to be updated to support cross-compilation. For
instance if the ``foo.exe`` program in the previous example was using
``Sys.os_type``, it should instead take it as a command line argument:

.. code:: lisp

  (rule (with-stdout-to blah (run ./foo.exe -os-type %{os_type})))
