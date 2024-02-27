#####################
 Findlib Integration
#####################

Dune integrates with findlib_ so that it is possible to use a dependency
built with Dune in a project that does not use Dune, or vice versa.

.. _findlib: https://github.com/ocaml/ocamlfind

.. seealso::

   :doc:`../explanation/ocaml-ecosystem` explains the role of
   findlib and its relation with Dune.

To do so, Dune both interprets and generates ``META`` files.

************************************
 How Dune Interprets ``META`` files
************************************

``META`` files use the concept of *predicates*, which can be used to
change the interpretation of the directives the files contain. However,
Dune does not expose this to the user.

Instead, Dune interprets ``META`` files assuming the following set of
predicates:

-  ``mt``: refers to a library that can be used with or without threads.
   Dune will force the threaded version.

-  ``mt_posix``: forces the use of POSIX threads rather than VM threads.
   VM threads are deprecated and will soon be obsolete.

-  ``ppx_driver``: when a library acts differently depending on whether
   it's linked as part of a driver or meant to add a ``-ppx`` argument
   to the compiler, choose the former behavior.

****************************************
 The Special Case of the OCaml Compiler
****************************************

Libraries installed by the compiler are a special case: when the OCaml
compiler is older than version 5.0, it does not include ``META`` files.
In that situation, Dune uses its own internal database.

***********************************
 How Dune Generates ``META`` Files
***********************************

When Dune builds a library, it generates a corresponding ``META`` file
automatically. Usually, there is nothing to do. However, for the rare
cases where a specific ``META`` file is needed, or to ease the
transition of a project to Dune, it is possible to write/generate a
specific one.

If a ``META.<package>.template`` is found in the same directory as
``<package>.OPAM``, the generated ``META.<package>`` file will be
produced by starting from the contents of ``META.<package>.template``
and replacing lines of the form ``# DUNE_GEN`` with the contents of the
``META`` it would normally generate.

For instance, to add ``field = "..."`` to the generated ``META`` file of
package ``pkg``, you can create a file named ``META.pkg.template`` with
the following contents:

.. code::

   # DUNE_GEN
   blah = "..."
