@check
======

This alias corresponds to the set of targets necessary for development tools to
work correctly. For example, it will build ``*.cmi``, ``*.cmt``, and ``*.cmti``
files so that Merlin and ``ocaml-lsp-server`` can be used in the project.
It is also useful in the development loop because it will catch compilation
errors without executing expensive operations such as linking executables.

Starting from ``(lang dune 3.23)``, this alias also generates and promotes a
``compile_commands.json`` file to the workspace root for projects that contain
C or C++ foreign stubs. This file is used by ``clangd`` and ``ccls`` for code
navigation and diagnostics. See :ref:`compile-commands` for details.

.. seealso:: :doc:`ocaml-index` for a fast feedback loop that
   also indexes the project.
