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
   :caption: Reference
   :maxdepth: 3

   concepts/scopes
   concepts/ordered-set-language
   concepts/boolean-language
   concepts/predicate-language
   concepts/variables
   concepts/library-dependencies
   concepts/preprocessing-spec
   concepts/dependency-spec
   concepts/ocaml-flags
   concepts/actions
   concepts/sandboxing
   concepts/locks
   concepts/promotion
   concepts/package-spec
   concepts/foreign
   formatting
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
   lexical-conventions
   opam
   toplevel-integration
   variants
   tests

.. toctree::
   :caption: Advanced topics

   advanced/meta-file-generation
   advanced/findlib-integration
   advanced/findlib-dynamic
   advanced/classical-ppx
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
