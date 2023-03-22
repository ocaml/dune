Findlib Integration
===================

Dune uses ``META`` files to support external libraries. However, it
doesn't export the full power of Findlib to the user, and it especially
doesn't let the user specify *predicates*.

This limitation is in place because they haven't been
needed thus far, and it would significantly complicate things to add full 
support for them. In particular, complex ``META`` files are often handwritten, and
the various features they offer are only available once the package is
installed, which goes against the root ideas Dune is built on.

In practice, Dune interprets ``META`` files, assuming the following
set of predicates:

- ``mt``: refers to a library that can be used
  with or without threads. Dune will force the threaded
  version.

- ``mt_posix``: forces the use of POSIX threads rather than VM
  threads. VM threads are deprecated and will soon be obsolete.

- ``ppx_driver``: when a library acts differently depending on whether
  it's linked as part of a driver or meant to add a ``-ppx`` argument
  to the compiler, choose the former behavior.

Note that Dune does not read *installed* ``META`` files for libraries
distributed with the compiler (as these files are not installed by the compiler
itself, but installed by `ocamlfind` and aren't always 
accurate). Instead, Dune uses its own internal database for this information.

