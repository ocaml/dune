***
FAQ
***

Why do many dune projects contain a Makefile?
=============================================

Many dune projects contain a toplevel `Makefile`. It is often only there for
convenience, for the following reasons:

1. there are many different build systems out there, all with a different CLI.
   If you have been hacking for a long time, the one true invocation you know is
   `make && make install`, possibly preceded by `./configure`

2. you often have a few common operations that are not part of the build and
   `make <blah>` is a good way to provide them

3. `make` is shorter to type than `dune build @install`

How to add a configure step to a dune project?
==============================================

The with-configure-step_ example shows one way to do it which
preserves composability; i.e. it doesn't require manually running `./configure`
script when working on multiple projects at the same time.

.. _with-configure-step: https://github.com/ocaml/dune/tree/master/example/sample-projects/with-configure-step

Can I use topkg with dune?
==========================

It's possible using the topkg-jbuilder_ but it's not recommended. dune-release_
subsumes topkg-jbuilder and is specifically tailored to dune projects.


How do I publish my packages with dune?
=======================================

Dune is just a build system and considers publishing outside of its scope.
However, the dune-release_ project is specifically designed for releasing dune
projects to opam. We recommend using tool for publishing dune packages.

here can I find some examples of projects using dune?
=====================================================

The dune-universe_ repository contains a snapshot of the latest versions of all
opam packages depending on dune. It is therefore a useful reference to
search through to find different approaches to constructing build rules.

.. _dune-universe: https://github.com/dune-universe/dune-universe
.. _topkg-jbuilder: https://github.com/samoht/topkg-jbuilder
.. _dune-release: https://github.com/samoht/dune-release
