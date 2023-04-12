.. dune documentation master file, created by
   sphinx-quickstart on Tue Apr 11 21:24:42 2017.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

Welcome to Dune's Documentation!
================================

.. We include the titles of the pages here to make sure they are in
   alphabetical order. Eventually we should name the files and titles
   similarly.

.. toctree::
   :caption: Getting Started and Core Concepts
   :maxdepth: 3

   overview
   quick-start
   dune-files
   usage

.. toctree::
   :caption: How-to Guides

   howto/formatting

.. toctree::
   :caption: Reference Guides
   :maxdepth: 3

   reference/lexical-conventions
   reference/ordered-set-language
   reference/boolean-language
   reference/predicate-language
   reference/library-dependencies
   reference/actions
   reference/foreign
   reference/cli
   reference/preprocessing-spec
   concepts/scopes
   concepts/variables
   concepts/dependency-spec
   concepts/ocaml-flags
   concepts/sandboxing
   concepts/locks
   concepts/promotion
   concepts/package-spec
   coq
   cross-compilation
   foreign-code
   caching
   dune-libs
   rpc
   documentation
   sites
   instrumentation
   jsoo
   opam
   toplevel-integration
   variants
   tests

.. toctree::
   :caption: Explanations

   explanation/preprocessing
   explanation/ocaml-ecosystem

.. toctree::
   :caption: Advanced topics

   advanced/meta-file-generation
   advanced/findlib-integration
   advanced/findlib-dynamic
   advanced/profiling-dune
   advanced/package-version
   advanced/ocaml-syntax
   advanced/variables-artifacts
   advanced/custom-cmxs

.. toctree::
   :caption: Miscellaneous
   :maxdepth: 3

   faq
   goals
   hacking
