A Tour of the Dune Codebase
===========================

.. note::

  This document is based on Dune 3.15.0, whose source can be browsed `here
  <https://github.com/ocaml/dune/tree/3.15.0>`_. The links in this tour point
  to this version, but will not reflect how this works in other versions of
  Dune.

Let's start with a very high level tour of how ``dune build`` operates.

As explained in :doc:`/explanation/mental-model`, ``dune build`` will interpret
the targets listed on the command line, interpret the ``dune`` files in the
workspace as rules, and execute the rules relevant to the requested targets.

These steps correspond to areas of the Dune codebase:

- the command-line interface is defined in :file:`bin/`;
- the ``dune`` files are interpreted using a library defined in
  :file:`src/dune_rules/`;
- they are registered into an engine in :file:`src/dune_engine/`.

Next, we will go deeper into these areas.

.. toctree::

  cli
  decoding
  rule-generation
  engine
  libraries
  vendor
  tests
