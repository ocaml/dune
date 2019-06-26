***********
Executables
***********

This section describes how to build and install binary programs with
Dune.

Definig executables
===================

TOOD.

Embedding build information into executables
============================================

Dune allows to embed build information such as versions in executbles
via the special ``dune.build-info`` library. This library exposes a
few informations about how the executable was built such as the
version of the project containing the executable or the list of
statically linked libraries with their versions. Printing the version
at which the current executable was built is as simple as:

.. code:: ocaml

          Printf.printf "version: %s\n"
            (match Build_info.V1.version with
             | None -> "n/a"
             | Some v -> Build_info.V1.Version.to_string v)

For libraries and executables from development repositories that don't
have version informations written directly in the ``dune-project``
file, the version is obtained by querying the version control
system. For instance, the following git command is used in git
repositories:

.. code:: bash

          git describe --always --dirty

which produces a human readable version string of the form
``<version>-<commits-since-version>-<hash>[-dirty]``.

Note that in the case where the version string is ontained from the
the version control system, the version string will only be written in
the binary once it is installed or promoted to the source tree. In
particular, if you evalute this expression as part of the build of
your package, it will return ``None``. This is to ensure that
committing does not hurt your development experience. Indeed, if dune
stored the version directly inside the freshly built binaries, then
everytime you commit your code the version would change and dune would
need to rebuild all the binaries and everything that depend on them,
such as tests. Instead Dune leaves a placeholder inside the binary and
fills it during installation or promotion.
