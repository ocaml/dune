.. _melange_main:

***********************************
JavaScript Compilation With Melange
***********************************

`Melange <https://github.com/melange-re/melange>`_ is a compiler from OCaml to
JavaScript. Unlike js_of_ocaml, Melange works by translating OCaml compiler
internal lambda representation to JS files. This allows to produce a single
JavaScript file from each OCaml module. Melange can be installed with
`opam, Esy, or Nix <https://github.com/melange-re/melange#installation>`_ package
managers.

Compiling to JS
===============

Dune has experimental support for building Melange libraries. To use it, you
must declare the ``melange`` extension in your ``dune-project`` file:

.. code:: scheme

  (lang dune 3.5)
  (using melange 0.1)

Then, given this example:

.. code:: bash

   echo 'Js.log "hello from melange"' > foo.ml

With the following ``dune`` file:

.. code:: scheme

  (library (name foo) (modes melange))

One can then request the ``.js`` target:

.. code:: bash

   $ dune build .foo.objs/melange/foo.js
   $ node _build/default/.foo.objs/melange/foo.js
   hello from melange

At the moment, executable targets are not supported, but using explicit
targets inside libraries as shown above should enable similar results.

Faster Builds with ``subdir`` and ``dirs`` Stanzas
==================================================

Melange libraries are commonly installed from the ``npm`` package repository,
together with other JavaScript packages. To avoid having Dune inspect
unnecessary folders in ``node_modules``, it is recommended to explicitly
include those folders that are relevant for Melange builds.

This can be accomplished by combining :ref:`subdir` and :ref:`dune-subdirs`
stanzas in a ``dune`` file and by co-locating this file together with the
``node_modules`` folder. The :ref:`dune-vendored_dirs` stanza can be used as
well to avoid warnings in Melange libraries during the application build.

.. code:: scheme

  (subdir
   node_modules
   (vendored_dirs reason-react)
   (dirs reason-react))
