.. _jsoo:

**********************
JavaScript compilation
**********************

js_of_ocaml_ is a compiler from OCaml to JavaScript. The compiler works by
translating OCaml bytecode to JS files. The compiler can be installed with opam:

.. code:: bash

   $ opam install js_of_ocaml-compiler

Compiling to JS
===============

Dune has full support building js_of_ocaml libraries and executables transparently.
There's no need to customize or enable anything to compile ocaml
libraries/executables to JS.

To build a JS executable, just define an executable as you would normally.
Consider this example:

.. code:: bash

   echo 'print_endline "hello from js"' > foo.ml

With the following dune file:

.. code:: scheme

  (executable (name foo) (modes js))

And then request the ``.js`` target:

.. code:: bash

   $ dune build ./foo.bc.js
   $ node _build/default/foo.bc.js
   hello from js

Similar targets are created for libraries, but we recommend sticking to the
executable targets.

Separate compilation
====================

Dune supports two modes of compilation

- Direct compilation of a bytecode program to JavaScript. This mode allows
  js_of_ocaml to perform whole program deadcode elimination and whole program
  inlining.

- Separate compilation, where compilation units are compiled to JavaScript
  separately and then linked together. This mode is useful during development as
  it builds more quickly.

The separate compilation mode will be selected when the build profile is
``dev``, which is the default. There is currently no other way to control this
behaviour.

.. _js_of_ocaml: http://ocsigen.org/js_of_ocaml/
