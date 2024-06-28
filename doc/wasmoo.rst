.. _wasmoo:

***************************************
Wasm Compilation With Wasm_of_ocaml
***************************************

.. TODO(diataxis)

   This is an how-to guide.

Wasm_of_ocaml_ is a compiler from OCaml to WebAssembly (Wasm for
short). The compiler works by translating OCaml bytecode to Wasm code.

Compiling to Wasm is very similar to compiling to JavaScript. See
:doc:`jsoo` for more information.


Compiling to Wasm
=================

Dune has full support building Wasm_of_ocaml libraries and executables transparently.
There's no need to customize or enable anything to compile OCaml
libraries/executables to Wasm.

To build a Wasm executable, just define an executable as you would normally.
Consider this example:

.. code:: console

   $ echo 'print_endline "hello from wasm"' > foo.ml

With the following ``dune`` file:

.. code:: dune

  (executable (name foo) (modes js) (js_of_ocaml (submodes wasm)))

And then request the ``.wasm.js`` target:

.. code:: console

   $ dune build ./foo.bc.wasm.js
   $ node _build/default/foo.bc.wasm.js
   hello from wasm

.. _wasm_of_ocaml: https://github.com/ocaml-wasm/wasm_of_ocaml
