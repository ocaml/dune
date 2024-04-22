The Engine
==========

The engine is the core, reusable part of Dune. It contains all the composable
primitives that make it a build system.

The fact that it is split from the :doc:`rules part <rule-generation>` makes it
possible to create a different build system using this library. For example,
Jane Street internally uses a build system with this engine as a backend, but a
different frontend and CLI.

In the context of Dune, the engine keeps track of the various directories and
the rules in them and is able to build files using them. In addition, it takes
care of the various caches that Dune uses, such as the one present in the
``_build`` directory, the :doc:`shared cache </caching>`, etc.
