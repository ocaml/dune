***
FAQ
***

Why Do Many Dune Projects Contain a ``Makefile``?
=================================================

Many Dune projects contain a root ``Makefile``. It's often only there for
convenience for the following reasons:

1. There are many different build systems out there, all with a different CLI.
   If you have been hacking for a long time, the one true invocation you know
   is ``make && make install``, possibly preceded by ``./configure``.

2. You often have a few common operations that aren't part of the build, so
   ``make <blah>`` is a good way to provide them.

3. ``make`` is shorter to type than ``dune build @install``

How to Add a Configure Step to a Dune Project
=============================================

The with-configure-step_ example shows one way to add a configure step that
preserves composability; i.e., it doesn't require manually running the
``./configure`` script when working on multiple projects simultaneously.

.. _with-configure-step: https://github.com/ocaml/dune/tree/master/example/with-configure-step.t

Can I Use ``topkg`` with Dune?
==============================

While it's possible to use the topkg-jbuilder_, it's not recommended.
dune-release_ subsumes ``topkg-jbuilder`` and is specifically tailored to Dune
projects.


How Do I Publish My Packages with Dune?
=======================================

Dune is just a build system and considers publishing outside of its scope.
However, the dune-release_ project is specifically designed for releasing Dune
projects to opam. We recommend using this tool for publishing Dune packages.

Where Can I Find Some Examples of Projects Using Dune?
======================================================

The dune-universe_ repository contains a snapshot of the latest versions of all
opam packages that depend on Dune. Therefore, it's a useful reference to find
different approaches for constructing build rules.

What is Jenga?
==============

jenga_ is a build system developed by Jane Street, mainly for internal use. It
was never usable outside of Jane Street, so it's not recommended for general
use. It has no relationship to Dune apart from Dune being the successor to
Jenga externally. Eventually, Dune is expected to replace Jenga internally at
Jane Street as well.

.. _dune-universe: https://github.com/dune-universe/dune-universe
.. _topkg-jbuilder: https://github.com/samoht/topkg-jbuilder
.. _dune-release: https://github.com/samoht/dune-release
.. _jenga: https://github.com/janestreet/jenga

How to Make Warnings Non-Fatal
==============================

`jbuilder` formerly displayed warnings, but most of them wouldn't stop the
build. However, Dune makes all warnings fatal by default. This can be a
challenge when porting a codebase to Dune. There are two ways to make warnings
non-fatal:

- The ``jbuilder`` compatibility executable works even with ``dune`` files. You
  can use it while some warnings remain and then switch over to the ``dune``
  executable. This is the recommended way to handle the situation.
- You can pass ``--profile release`` to ``dune``. It will set up different
  compilation options that usually make sense for release builds, including
  making warnings non-fatal. This is done by default when installing packages
  from opam.
- You can change the flags used by the ``dev`` profile by adding the following
  stanza to a ``dune`` file:

.. code:: scheme

  (env
    (dev
      (flags (:standard -warn-error -A))))

How to Display the Output of Commands as They Run
=================================================

When Dune runs external commands, it redirects and saves their output, then
displays it when complete. This ensures that there's no interleaving when
writing to the console.

But this might not be what the you want. For example, when you debug a hanging
build.

In that case, one can pass ``-j1 --no-buffer`` so the commands are directly
printed on the console (and the parallelism is disabled so the output stays
readable).

How Can I Generate an ``mli`` File From an ``ml`` File
======================================================

When a module starts as just an implementation (``.ml`` file), it can be
tedious to define the corresponding interface (``.mli`` file).

It is possible to use the ``ocaml-print-intf`` program (available on opam
through ``$ opam install ocaml-print-intf``) to generate the right ``mli``
file:

.. code:: bash

  $ dune exec -- ocaml-print-intf ocaml_print_intf.ml
  val root_from_verbose_output : string list -> string
  val target_from_verbose_output : string list -> string
  val build_cmi : string -> string
  val print_intf : string -> unit
  val version : unit -> string
  val usage : unit -> unit

The ``ocaml-print-intf`` program has special support for Dune, so it will
automatically understand external dependencies.
