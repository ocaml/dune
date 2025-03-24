.. _wasmoo:

***************************************
Wasm Compilation With Wasm_of_ocaml
***************************************

.. TODO(diataxis)

   This is an how-to guide.

Wasm_of_ocaml is a compiler from OCaml to WebAssembly (Wasm for short). The
compiler works by translating OCaml bytecode to Wasm code. The compiler can
be installed with opam:

.. code:: console

   $ opam install wasm_of_ocaml-compiler

Compiling to Wasm
=================

Dune has full support for building wasm_of_ocaml libraries and executables transparently.
There's no need to customise or enable anything to compile OCaml
libraries/executables to Wasm.

To build a Wasm executable, just define an executable as you would normally.
Consider this example:

.. code:: console

   $ echo 'print_endline "hello from wasm"' > foo.ml

With the following ``dune`` file:

.. code:: dune

   (executable (name foo) (modes wasm))

And then request the ``.wasm.js`` target:

.. code:: console

    $ dune build ./foo.bc.wasm.js
    $ node _build/default/foo.bc.wasm.js
    hello from wasm

If you're using the js_of_ocaml syntax extension, you must remember to add the
appropriate PPX in the ``preprocess`` field:

.. code:: dune

   (executable
    (name foo)
    (modes wasm)
    (preprocess (pps js_of_ocaml-ppx)))

Selective Compilation
=====================

The ``js`` and ``wasm`` modes can be selectively disabled using the ``(js_of_ocaml (enabled_if ...))`` and ``(wasm_of_ocaml (enabled_if ...))`` options. This allows for instance to generate one, or the other dependings on a profile:

.. code:: dune

   (env
    (js-only
     (wasm_of_ocaml
      (enabled_if false)))
    (wasm-only
     (js_of_ocaml
      (enabled_if false))))

To be able to invoke the generated code using the same JavaScript script name in all cases, you can add a rule to copy the Wasm launcher script when the js_of_ocaml compilation is disabled.

.. code:: dune

   (rule
    (action
     (copy foo.bc.wasm.js foo.bc.js))
    (enabled_if
     (= %{profile} wasm-only)))

Separate Compilation
====================

Dune supports two modes of compilation:

- Direct compilation of a bytecode program to Wasm. This mode allows
  wasm_of_ocaml to perform whole-program deadcode elimination and whole-program
  inlining.

- Separate compilation, where compilation units are compiled to Wasm
  separately and then linked together. This mode is useful during development as
  it builds more quickly.

The separate compilation mode will be selected when the build profile
is ``dev``, which is the default. It can also be explicitly specified
in an ``env`` stanza (see :doc:`/reference/dune/env`) or per executable
inside ``(wasm_of_ocaml (compilation_mode ...))`` (see :doc:`/reference/dune/executable`)

Sourcemap
=========

Wasm_of_ocaml can generate sourcemaps for the generated Wasm code.
By default, they are generated when using the ``dev`` build profile and are not generated otherwise.
The behavior can explicitly be specified in an ``env`` stanza (see :doc:`/reference/dune/env`)
or per executable inside ``(wasm_of_ocaml (sourcemap ...))`` (see :doc:`/reference/dune/executable`)
