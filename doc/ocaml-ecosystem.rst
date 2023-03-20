The OCaml ecosystem
===================

The OCaml ecosystem is not monolithic: the compiler and tools are not
maintained by the same entities. As such, it can be difficult to understand the
history and roles of the various pieces of this ecosystem. The goal of this
page is to give a quick overview of the situation and of the role that Dune
plays in it.

The compiler distribution: compiling and linking
------------------------------------------------

The compiler distribution contains "core" tools including the compilers
(``ocamlc`` and ``ocamlopt``). They turn source files (with extensions ``.ml``
and ``.mli``) into executables and libraries. Dependencies between compiled
objects only exist at the module level, so this is a low-level tool.

Findlib: metadata for libraries
-------------------------------

Findlib is a tool that defines the concept of library, so that libraries can
depend on other libraries on top of the notion of module. Definitions of
libraries, and other pieces of metadata, are stored in ``META`` files.

Findlib ships an executable named ``ocamlfind`` that can be used as a wrapper
on top of the compilers, to perform tasks such as producing an executable from
compiled object files and external libraries.

OPAM: a collection of software projects
---------------------------------------

OPAM is a package manager. It is used to determine which packages are
necessary, and how to fetch and build them. Packages can contain libraries,
executables, and other kinds of files.

The notion of version is specific to opam: if your project uses a function
named ``Png.read_file`` but this function has been added only in version
``1.2.0`` of that package, opam needs to know about it.

OPAM manages collections of installed packages, called switches. Using your
project's dependencies (names and version constraints), it is able to create a
switch that you'll be using to develop your project.

Public definition of packages are available in a database called
opam-repository which is maintained as a public git repository. Publishing a
package on opam (to make sure that external users can use your project)
consists in adding its definition to opam-repository.

Dune: giving structure to your source tree
------------------------------------------

Dune is a build system. It is used to orchestrate the compilation of source
files into executables and libraries.

Assuming you have a development switch set up, you communicate to dune how your
project is organized in terms of executables, libraries and tests. It is then able to assemble the source files of your projects with the dependencies installed in an opam switch, to create compiled assets for your project.

How Dune integrates with the ecosystem
--------------------------------------

Dune is designed to integrate with this set of tools:

- by knowing how the compilers operate, it knows which build commands should be
  re-executed if some source files change.
- it outputs metadata like dependency information into ``META`` files that
  findlib is able to use. This ensures that a project that does not use dune
  can use a library that has been produced by Dune. Conversely, it can read
  these files to determine dependency information for dependencies that have
  not been produced by Dune.
- it is able to generate opam files with filenames consistents with how opam
  looks for them. The generated files use build commands that make use of the
  ``@install`` and ``@runtest`` alias so that the dune abstractions map to the
  opam ones.

Dune is opinionated
-------------------

As described above, the OCaml ecosystem does not have a centralized toolchain.
Units such as modules, libraries, and packages, operate at different levels and
the relation between these can be confusing to users.

Dune tries to simplify the picture by reducing the difference between these
objects:

- by default, a library will only expose a single top-level module named after
  the library.
- a library can only be installed in the package of the same name. This means
  that the names found in ``dune-project`` and ``opam`` files (package names)
  are consistent with the names found in ``dune`` files (library names).
