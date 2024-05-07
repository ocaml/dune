@check
======

This alias corresponds to the set of targets necessary for development tools to
work correctly. For example, it will build ``*.cmi``, ``*.cmt``, and ``*.cmti``
files so that Merlin and ``ocaml-lsp-server`` can be used in the project.
It is also useful in the development loop because it will catch compilation
errors without executing expensive operations such as linking executables.

.. seealso:: :doc:`ocaml-index` for a fast feedback loop that
   also indexes the project.
