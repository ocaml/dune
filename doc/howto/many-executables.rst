How to Organize a Project of Many Executables
=============================================

Suppose that you're following a tutorial that has these chapters, each with many separate OCaml programs:

- Introducing OCaml
- Introducing Lwt
- Introducing Preprocessor Extensions

To organize those programs, add executables to a dune project with ``dune init
executable``, providing both the name and path arguments:

.. code:: shell-session

  $ dune init exe hello intro_ocaml
  Success: initialized executable component named hello

  $ dune init exe fibonacci intro_ocaml
  Success: initialized executable component named fibonacci

  $ dune init exe watchfs intro_lwt --libs=lwt.unix
  Success: initialized executable component named watchfs

  $ dune init exe filescan intro_ppx --ppx=ppx_defer
  Success: initialized executable component named filescan

Added Folder Structure
----------------------

.. code:: shell-session

   ├── intro_lwt
   │   ├── dune
   │   └── watchfs.ml
   ├── intro_ocaml
   │   ├── dune
   │   ├── fibonacci.ml
   │   └── hello.ml
   ├── intro_ppx
   │   ├── dune
   │   └── filescan.ml

You can execute these by including the path:

.. code:: shell-session

  $ dune exec intro_ocaml/hello.exe
  Hello, World!

Adding Libraries
----------------

As you begin to share modules between executables, you can add them individually:

.. code:: shell-session

  $ dune init lib greet lib/util
  Success: initialized library component named greet

::::{dropdown} `lib/util/greet.ml`
:icon: file-code

:::{literalinclude} many-executables/greet.ml
:language: ocaml
:::

::::

Or you can, as with the default ``dune init proj`` layout, add a directory of submodules:

.. code:: shell-session

  $ dune init lib intro_ocaml lib/intro_ocaml
  Success: initialized library component named intro_ocaml

::::{dropdown} `lib/intro_ocaml/magic.ml`
:icon: file-code

:::{literalinclude} many-executables/magic.ml
:language: ocaml
:::

::::

::::{dropdown} `lib/intro_ocaml/math.ml`
:icon: file-code

:::{literalinclude} many-executables/math.ml
:language: ocaml
:::

::::

Which you can use like normal, adding the named libraries to an executable's stanza:

::::{dropdown} `intro_ocaml/dune`
:icon: file-code

:::{literalinclude} many-executables/intro-dune
:language: dune
:emphasize-lines: 3
:::

::::

::::{dropdown} `intro_ocaml/hello.ml`
:icon: file-code

:::{literalinclude} many-executables/hello.ml
:language: ocaml
:::

::::

.. code:: shell-session

  $ dune exec intro_ocaml/hello.exe
  Hi world
  61456
