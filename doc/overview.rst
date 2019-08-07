********
Overview
********

Dune is a build system for OCaml and Reason. It is not intended as a
completely generic build system that is able to build any given project
in any language. On the contrary, it makes lots of choices in order to
encourage a consistent development style.

This scheme is inspired from the one used inside Jane Street and adapted
to the opam world. It has matured over a long time and is used daily by
hundred of developers, which means that it is highly tested and
productive.

When using dune, you give very little and high-level information to
the build system, which in turn takes care of all the low-level
details, from the compilation of your libraries, executables and
documentation, to the installation, setting up of tests, setting up of
the development tools such as merlin, etc.

In addition to the normal features one would expect from a build system
for OCaml, dune provides a few additional ones that detach it from
the crowd:

-  you never need to tell dune where things such as libraries are.
   Dune will always discover them automatically. In particular, this
   means that when you want to re-organize your project you need to do no
   more than rename your directories, dune will do the rest

-  things always work the same whether your dependencies are local or
   installed on the system. In particular, this means that you can always
   drop in the source for a dependency of your project in your working
   copy and dune will start using it immediately. This makes dune a
   great choice for multi-project development

-  cross-platform: as long as your code is portable, dune will be
   able to cross-compile it (note that dune is designed internally
   to make this easy but the actual support is not implemented yet)

-  release directly from any revision: dune needs no setup stage. To
   release your project, you can simply point to a specific tag. You can
   of course add some release steps if you want to, but it is not
   necessary

This section gives simple usage examples of dune. You can also look at
`examples <https://github.com/ocaml/dune/tree/master/example>`__ for complete
examples of projects using dune.

Project Layout
==============

A typical dune project will have a ``dune-project`` and one or more
``<package>.opam`` file at the root as well as ``dune`` files wherever
interesting things are: libraries, executables, tests, documents to install,
etc...

It is recommended to organize your project so that you have exactly one library
per directory. You can have several executables in the same directory, as long
as they share the same build configuration. If you'd like to have multiple
executables with different configurations in the same directory, you will have
to make an explicit module list for every executable using ``modules``.

Terminology
===========

-  **package**: a package is a set of libraries, executables, ... that
   are built and installed as one by opam

-  **project**: a project is a source tree, maybe containing one or more
   packages

-  **root**: the root is the directory from where dune can build
   things. Dune knows how to build targets that are descendants of
   the root. Anything outside of the tree starting from the root is
   considered part of the **installed world**. How the root is
   determined is explained in :ref:`finding-root`.

-  **workspace**: the workspace is the subtree starting from the root.
   It can contain any number of projects that will be built
   simultaneously by dune

-  **installed world**: anything outside of the workspace, that dune
   takes for granted and doesn't know how to build

-  **installation**: this is the action of copying build artifacts or
   other files from the ``<root>/_build`` directory to the installed
   world

-  **scope**: a scope determines where private items are
   visible. Private items include libraries or binaries that will not
   be installed. In dune, scopes are sub-trees rooted where at
   least one ``<package>.opam`` file is present. Moreover, scopes are
   exclusive. Typically, every project defines a single scope. See
   :ref:`scopes` for more details

-  **build context**: a build context is a subdirectory of the
   ``<root>/_build`` directory. It contains all the build artifacts of
   the workspace built against a specific configuration. Without
   specific configuration from the user, there is always a ``default``
   build context, which corresponds to the environment in which dune
   is executed. Build contexts can be specified by writing a
   :ref:`dune-workspace` file

-  **build context root**: the root of a build context named ``foo`` is
   ``<root>/_build/<foo>``

-  **alias**: an alias is a build target that doesn't produce any file and has
   configurable dependencies. Aliases are per-directory. However, on the command
   line, asking for an alias to be built in a given directory will trigger the
   construction of the alias in all children directories recursively. Dune
   defines several :ref:`builtin-aliases`.

- **environment**: in dune, each directory has an environment
  attached to it. The environment determines the default values of
  various parameters, such as the compilation flags. Inside a scope,
  each directory inherit the environment from its parent. At the root
  of every scope, a default environment is used. At any point, the
  environment can be altered using an :ref:`dune-env` stanza.

- **build profile**: a global setting that influence various
  defaults. It can be set from the command line using ``--profile
  <profile>`` or from ``dune-workspace`` files. The following
  profiles are standard:

  -  ``release`` which is the profile used for opam releases
  -  ``dev`` which is the default profile when none is set explicitly, it
     has stricter warnings that the ``release`` one

Building a hello world program
==============================

In a directory of your choice, write this ``dune`` file:

.. code:: scheme

    ;; This declares the hello_world executable implemented by hello_world.ml
    (executable
     (name hello_world))

This ``hello_world.ml`` file:

.. code:: ocaml

    print_endline "Hello, world!"

And build it with:

.. code:: bash

    dune build hello_world.exe

The executable will be built as ``_build/default/hello_world.exe``. Note that
native code executables will have the ``.exe`` extension on all platforms
(including non-Windows systems). The executable can be built and run in a single
step with ``dune exec ./hello_world.exe``.

Building a hello world program using Lwt
========================================

In a directory of your choice, write this ``dune`` file:

.. code:: scheme

    (executable
     (name hello_world)
     (libraries lwt.unix))

This ``hello_world.ml`` file:

.. code:: ocaml

    Lwt_main.run (Lwt_io.printf "Hello, world!\n")

And build it with:

.. code:: bash

    dune build hello_world.exe

The executable will be built as ``_build/default/hello_world.exe``

Building a hello world program using Core and Jane Street PPXs
==============================================================

Write this ``dune`` file:

.. code:: scheme

    (executable
     (name hello_world)
     (libraries core)
     (preprocess (pps ppx_jane)))

This ``hello_world.ml`` file:

.. code:: ocaml

    open Core

    let () =
      Sexp.to_string_hum [%sexp ([3;4;5] : int list)]
      |> print_endline

And build it with:

.. code:: bash

    dune build hello_world.exe

The executable will be built as ``_build/default/hello_world.exe``

Defining a library using Lwt and ocaml-re
=========================================

Write this ``dune`` file:

.. code:: scheme

    (library
     (name        mylib)
     (public_name mylib)
     (libraries re lwt))

The library will be composed of all the modules in the same directory.
Outside of the library, module ``Foo`` will be accessible as
``Mylib.Foo``, unless you write an explicit ``mylib.ml`` file.

You can then use this library in any other directory by adding ``mylib``
to the ``(libraries ...)`` field.

Setting the OCaml compilation flags globally
============================================

Write this ``dune`` file at the root of your project:

.. code:: scheme

    (env
     (dev
      (flags (:standard -w +42)))
     (release
      (flags (:standard -O3))))

`dev` and `release` correspond to build profiles. The build profile
can be selected from the command line with `--profile foo` or from a
`dune-workspace` file by writing:

.. code:: scheme

    (profile foo)

Using cppo
==========

Add this field to your ``library`` or ``executable`` stanzas:

.. code:: scheme

    (preprocess (action (run %{bin:cppo} -V OCAML:%{ocaml_version} %{input-file})))

Additionally, if you are include a ``config.h`` file, you need to
declare the dependency to this file via:

.. code:: scheme

    (preprocessor_deps config.h)

Using the .cppo.ml style like the ocamlbuild plugin
---------------------------------------------------

Write this in your ``dune`` file:

.. code:: scheme

    (rule
     (targets foo.ml)
     (deps    (:first-dep foo.cppo.ml) <other files that foo.ml includes>)
     (action  (run %{bin:cppo} %{first-dep} -o %{targets})))

Defining a library with C stubs
===============================

Assuming you have a file called ``mystubs.c``, that you need to pass
``-I/blah/include`` to compile it and ``-lblah`` at link time, write
this ``dune`` file:

.. code:: scheme

    (library
     (name            mylib)
     (public_name     mylib)
     (libraries       re lwt)
     (c_names         mystubs)
     (c_flags         (-I/blah/include))
     (c_library_flags (-lblah)))

Defining a library with C stubs using pkg-config
================================================

Same context as before, but using ``pkg-config`` to query the
compilation and link flags. Write this ``dune`` file:

.. code:: scheme

    (library
     (name            mylib)
     (public_name     mylib)
     (libraries       re lwt)
     (c_names         mystubs)
     (c_flags         (:include c_flags.sexp))
     (c_library_flags (:include c_library_flags.sexp)))

    (rule
     (targets c_flags.sexp c_library_flags.sexp)
     (deps    (:discover config/discover.exe))
     (action  (run %{discover} -ocamlc %{OCAMLC})))

Then create a ``config`` subdirectory and write this ``dune`` file:

.. code:: scheme

    (executable
     (name discover)
     (libraries dune.configurator))

as well as this ``discover.ml`` file:

.. code:: ocaml

    module C = Configurator.V1

    let () =
    C.main ~name:"foo" (fun c ->
    let default : C.Pkg_config.package_conf =
      { libs   = ["-lgst-editing-services-1.0"]
      ; cflags = []
      }
    in
    let conf =
      match C.Pkg_config.get c with
      | None -> default
      | Some pc ->
         match (C.Pkg_config.query pc ~package:"gst-editing-services-1.0") with
         | None -> default
         | Some deps -> deps
    in


    C.Flags.write_sexp "c_flags.sexp"         conf.cflags;
    C.Flags.write_sexp "c_library_flags.sexp" conf.libs)


Using a custom code generator
=============================

To generate a file ``foo.ml`` using a program from another directory:

.. code:: scheme

    (rule
     (targets foo.ml)
     (deps    (:gen ../generator/gen.exe))
     (action  (run %{gen} -o %{targets})))

Defining tests
==============

Write this in your ``dune`` file:

.. code:: scheme

    (test (name my_test_program))

And run the tests with:

.. code:: bash

    dune runtest

It will run the test program (the main module is ``my_test_program.ml``) and
error if it exits with a nonzero code.

In addition, if a ``my_test_program.expected`` file exists, it will be compared
to the standard output of the test program and the differences will be
displayed. It is possible to replace the ``.expected`` file with the last output
using:

.. code:: bash

    dune promote

Building a custom toplevel
==========================

A toplevel is simply an executable calling ``Topmain.main ()`` and linked with
the compiler libraries and ``-linkall``. Moreover, currently toplevels can only
be built in bytecode.

As a result, write this in your ``dune`` file:

.. code:: scheme

    (executable
     (name       mytoplevel)
     (libraries  compiler-libs.toplevel mylib)
     (link_flags (-linkall))
     (modes      byte))

And write this in ``mytoplevel.ml``

.. code:: ocaml

    let () = Topmain.main ()
