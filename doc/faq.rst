***
FAQ
***

.. TODO(diataxis)

   This is an odd one - most of these questions are not frequently asked at all.

   Some of these are mini how-to guides, or sections of existing guides.

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

.. code:: dune

  (env
    (dev
      (flags (:standard -warn-error -A))))

How to Turn Specific Errors into Warnings
=========================================

Dune is strict about warnings by default in that all warnings are treated as
fatal errors. To change certain errors into warnings for a project, you can add
the following to ``dune-workspace``:

.. code:: dune

  (env (dev (flags :standard -warn-error -27-32)))

In this example, the warnings 27 (unused-var-strict) and 32
(unused-value-declaration) are treated as warnings rather than errors.

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

.. code:: console

  $ dune exec -- ocaml-print-intf ocaml_print_intf.ml
  val root_from_verbose_output : string list -> string
  val target_from_verbose_output : string list -> string
  val build_cmi : string -> string
  val print_intf : string -> unit
  val version : unit -> string
  val usage : unit -> unit

The ``ocaml-print-intf`` program has special support for Dune, so it will
automatically understand external dependencies.

How Can I Build a Single Library?
=================================

You might want to do this when you don't have all the dependencies installed to
compile an entire project, or parts of the project don't build for whatever
reason. Maybe you want to check if your changes compile or produce build
artifacts needed by ``ocaml-lsp-server``.

Suppose you have a library defined in ``src/foo/dune``:

.. code:: dune

  (library
   (public_name my_library)
   ...)

You can build this library on its own by running the following from the project
root directory:

.. code:: console

   $ dune build %{cmxa:src/foo/my_library}

Note that the path (``src/foo`` in the example above) is relative to the current
directory - not the project root. If the library defines a ``name`` distinct from
its ``public_name`` then that can be used interchangeably with the ``public_name``
in this command.

Files and Directories Whose Names Begin with "." (Period) are Ignored by ``source_tree``
========================================================================================

Dune's default behaviour is to ignore files and directories starting with "."
when copying directories with ``source_tree``. This is to avoid accidentally
copying the ``.git`` directory into the ``_build`` directory during a build.

This is a common source of confusion when interoperating with other libraries
that use hidden directories for configuration, such as Rust. For example
consider this rule which builds a Rust library contained in a subdirectory
foo-rs:

.. code:: dune

  (rule
   (target foo.a)
   (deps
    (source_tree foo-rs))
   (action
    (progn
     (chdir
      foo-rs
      (run cargo build --release))
     (run mv foo-rs/target/release/%{target} %{target}))))

The build config for the Rust project will be in a directory
``foo-rs/.cargo/config.toml``, and by default the ``.cargo`` directory won't
get copied into the ``_build`` directory and so the Rust project will build
with an incorrect configuration.

To fix this, create a ``dune`` file at the top level of the Rust project (i.e.,
``foo-rs/dune``):

.. code:: dune

    (dirs :standard .cargo)

If you're following the standard advice for embedding Rust projects into Dune
projects then you likely already have a ``dune`` project inside your Rust
project that looks like:

.. code:: dune

    (dirs :standard \ target)
    (data_only_dirs vendor)

In this case you can update it to look like this:

.. code:: dune

    (dirs :standard .cargo \ target)
    (data_only_dirs vendor)

How Can I Write Inline Tests in a Package Without my Users Needing to Install ``ppx_inline_test``?
==================================================================================================

If you came to OCaml from Rust and noticed that Dune has a feature for running
inline tests you might be wondering how to do the OCaml equivalent of:

.. code:: rust

   // define a private function
   fn foo() { ... }

   // test the function right next to its definition
   #[test]
   fn test_of_foo() { ... }

That is, writing tests for private functions right next to the definition of
those functions. The :ref:`inline_tests` documentation describes how to do this
using the ``ppx_inline_test`` package; however, if you do this in your package,
then your package must `unconditionally` depend on the ``ppx_inline_test``
package. Opam has a notion of test-only dependencies (its ``with-test`` flag),
but you cannot use this with ``ppx_inline_test``. The consequence of this is
that anyone depending on your package is also transitively depending on
``ppx_inline_test`` as well as all of its dependencies.

The reason for this is OCaml code with preprocessor directives (such as those
used for inline tests with ``ppx_inline_test``) is technically not valid OCaml
code until it has been preprocessed. Unlike the cargo build system used for
Rust, Dune does not have a preprocessor built into it. Instead, it relies on
external tools (such as ``ppx_inline_test``) to parse the code and replace any
preprocessor directives with valid OCaml. Dune doesn't know how to parse OCaml
code at all so it can't even remove inline tests from the code in cases where
``ppx_inline_test`` is unavailable.

The blessed workaround for folks who want to use ``ppx_inline_test`` in their
packages but don't want to add it as a dependency is to create a new
(unreleased) package which contains all the tests. In the original package,
expose all the private APIs you intend to test via public modules named
something foreboding such as ``For_test`` so your users know not to rely on
their contents and then have the test package define tests that call your
"private" APIs through the ``For_test`` modules.
