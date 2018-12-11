****
opam
****

opam_ is the official package manager for OCaml, and dune offers some
integration with it.

Generating opam files
=====================

Dune is able to use metadata specified in the ``dune-project`` file to cross
reference it with the information in the user written ``.opam`` file. To enable
this integration, a user needs to add an ``(opam ..)`` field to the dune-project
file.

The fields that dune uses for this purpose are:

- ``(license <name>)`` - Specified the license of the project

- ``(authors <authors>)`` - A list of authors

- ``(source <source>)`` - where the source is specified two ways:
  ``(github <user/repo>)`` or ``(uri <uri>)``

To enable dune suggesting corrections to the opam stanza, the user must specify
an ``(opam <fields>)`` with the fields:

- ``(tags <tags>)`` - Specify the list of tags for all packages
- ``(depends <dep-specification>)`` - The list of dependencies shared by all opam packages
  in this dune project
- ``(conflicts <dep-specification>)`` - The list of conflicts shared by all opam
  packages in this dune project
- ``(package <package>)`` - the list of packages in this project and their
  individual metadata.

The list of dependencies ``<dep-specification>`` is modeled after opam's own
language: The syntax is as a list of the following elements:

.. code::
   op := '=' | '<' | '>' | '<>' | '>=' | '<='

   stage := :with_test | :build | :dev

   constr := (<op> <version>)

   logop := or | and

   dep := (name <stage>)
        | (name <constr>)
        | (name (<logop> (<stage> | <constr>)*))

   dep-specification = dep+

The `(package <package>)` field contains the fields:

- ``(name <string>)`` is the name of the package

- ``(synopsis <string>)`` is a short package description

- ``(description <string>)`` is a longer package description

- ``(depends <dep-specification>)`` are package specific dependencies

- ``(conflicts <dep-specification)`` are package specific conflicts

Here's a complete example of a dune file with opam metadata specification:

.. code:: scheme

   (lang dune 1.10)
   (name cohttp)
   (source (github mirage/ocaml-cohttp))
   (license ISC)
   (authors "Anil Madhavapeddy" "Rudi Grinberg")

   (opam
     (tags org:mirage org:dune)
     (depends
     (ocaml (>= 4.06.0))
     (cohttp (>= 1.0.0)))
     (package
       (name cohttp)
       (synopsis "An OCaml library for HTTP clients and servers")
       (description "A longer description")
       (depends
         (alcotest :with-test)
         (dune (and :build (> 1.5)))
         (foo (and :dev (> 1.5) (< 2.0)))
         (uri (>= 1.9.0))
         (uri (< 2.0.0))
         (fieldslib (> v0.12))
         (fieldslib (< v0.13))))
     (package
       (name cohttp-async)
       (synopsis "HTTP client and server for the Async library")
       (description "A _really_ long description")
       (depends
         (cohttp (>= 1.0.2))
         (conduit-async (>= 1.0.3))
         (async (>= v0.10.0))
         (async (< v0.12)))))

.. _opam: https://opam.ocaml.org/
