Vendored Libraries
==================

As an opam package, Dune has no dependencies. But it uses some existing
libraries by copying, or "vendoring", their source code into the
:file:`vendor/` directory.

In some cases, the external dependency is extracted from the upstream
repository. In other cases, we carry patches and refer to a fork in the
`ocaml-dune GitHub organization <https://github.com/ocaml-dune>`_.

The source code in the :file:`vendor/` directory is not meant to be edited
directly. Instead, it is edited in the external repository, and the copy in the
Dune source tree is updated by running an update script, such as
`update-spawn.sh <https://github.com/ocaml/dune/blob/3.15.0/vendor/update-spawn.sh>`_.
