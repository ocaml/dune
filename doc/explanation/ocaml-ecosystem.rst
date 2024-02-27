#####################
 The OCaml Ecosystem
#####################

The OCaml ecosystem is not monolithic: the compiler and tools are not
maintained by the same entities. As such, it can be difficult to
understand the history and roles of the various pieces of this
ecosystem. The goal of this page is to give a quick overview of the
situation and the role that Dune plays in it.

********************************************************
 The OCaml Compiler Distribution: Compiling and Linking
********************************************************

The `OCaml compiler distribution <https://github.com/ocaml/ocaml>`_
contains "core" tools including the compilers (``ocamlc`` and
``ocamlopt``). They turn source files (with extensions ``.ml`` and
``.mli``) into executables and libraries. Dependencies between compiled
objects only exist at the module level, so this is a low-level tool.

*********************************
 Findlib: Metadata for Libraries
*********************************

Findlib_ is a tool that defines the concept of library, so that
libraries can depend on other libraries on top of the notion of module.
Definitions of libraries, and other pieces of metadata, are stored in
``META`` files.

Findlib ships an executable named ``ocamlfind`` that can be used as a
wrapper on top of the compilers to perform tasks such as producing an
executable from compiled object files and external libraries.

.. _findlib: https://github.com/ocaml/ocamlfind

*****************************************
 Opam: a Collection of Software Projects
*****************************************

Opam is a package manager. It is used to determine which packages are
necessary, and how to fetch and build them. Packages can contain
libraries, executables, and other kinds of files.

The notion of version is specific to opam. If your project uses a
function named ``Png.read_file`` but this function has been added only
in version ``1.2.0`` of that package, opam needs to know about it.

Opam manages collections of installed packages, called switches. Using
your project's dependencies (names and version constraints), it is able
to create a switch that you'll be using to develop your project.

Public definitions of packages are available in a database called
``opam-repository`` which is maintained as a public Git repository.
Publishing a package on opam (to make sure that external users can use
your project) consists in adding its definition to ``opam-repository``.

********************************************
 Dune: Giving Structure to Your Source Tree
********************************************

Dune is a build system. It is used to orchestrate the compilation of
source files into executables and libraries.

Assuming you have a development switch set up, you communicate to Dune
about how your project is organized in terms of executables, libraries,
and tests. It is then able to assemble the source files of your
projects, with the dependencies installed in an opam switch, to create
compiled assets for your project.

****************************************
 How Dune Integrates With the Ecosystem
****************************************

Dune is designed to integrate with the tools mentioned above:

-  By knowing how the OCaml compilers operate, it knows which build
   commands should be re-executed if some source files change.

-  It outputs metadata like dependency information into ``META`` files
   that Findlib is able to make use of. This ensures that even if a
   project does not use Dune, it can use a library that has been
   produced by Dune. Conversely, it can read these files to determine
   dependency information for dependencies that have not been produced
   by Dune.

-  It is able to generate opam files with filenames consistent with how
   opam looks for them. The generated files use build commands that make
   use of the ``@install`` and ``@runtest`` :term:`aliases <alias>` so
   that the Dune abstractions map to the opam ones.

*********************
 Dune is Opinionated
*********************

As described above, the OCaml ecosystem does not have a centralized
toolchain. Units such as modules, libraries, and packages operate at
different levels, and the relation between these can be confusing to
users.

Dune tries to simplify the picture by reducing the difference between
these objects:

-  By default, a library will only expose a single top-level module
   named after the library (this is called a wrapped library).

-  A library can only be installed in the package of the same name. This
   means that the names found in ``dune-project`` and ``opam`` files
   (package names) are consistent with the names found in ``dune`` files
   (library names). More precisely, libraries ``foo``, ``foo.bar`` and
   ``foo.baz`` are part of the ``foo`` package.
