**********
Quickstart
**********

.. TODO(diataxis)

   Split this into:

   - :doc:`tutorials/from-zero-to-opam`
   - :doc:`tutorials/developing-with-dune`
   - :doc:`howto/changing-flags`
   - an how-to guide about ``cppo``
   - an how-to guide about staged programming / generators
   - an how-to guide about testing

This document gives simple usage examples of Dune. You can also look at
`examples <https://github.com/ocaml/dune/tree/master/example>`__ for complete
examples of projects using Dune with `CRAM stanzas <https://ocaml.org/p/craml/1.0.0>`__.

To try these examples, you will need to have Dune installed. See
:doc:`howto/install-dune`.

Initializing Projects
=====================

The following subsections illustrate basic usage of the ``dune init proj``
subcommand. For more documentation, see :ref:`initializing_components` and the
inline help available from ``dune init --help``.

.. _initializing-an-executable:

Initializing an Executable
--------------------------

To initialize a project that will build an executable program, run the following
(replacing ``project_name`` with the name of your project):

.. code:: console

  $ dune init proj project_name

This creates a project directory that includes the following contents:

.. code::

    project_name/
    ├── dune-project
    ├── test
    │   ├── dune
    │   └── test_project_name.ml
    ├── lib
    │   └── dune
    ├── bin
    │   ├── dune
    │   └── main.ml
    └── project_name.opam

Now, enter your project's directory:

.. code:: console

  $ cd project_name

Then, you can build your project with:

.. code:: console

  $ dune build

You can run your tests with:

.. code:: console

  $ dune test

You can run your program with:

.. code:: console

  $ dune exec project_name

This simple project will print "Hello World" in your shell.

The following itemization of the generated content isn't necessary to review at
this point. But whenever you are ready, it will provide jump-off points from
which you can dive deeper into Dune's capabilities:

* The ``dune-project`` file specifies metadata about the project, including its
  name, packaging data (including dependencies), and information about the
  authors and maintainers. Open this in your editor to fill in the
  placeholder values. See :doc:`/reference/dune-project/index` for
  details.
* The ``test`` directory contains a skeleton for your project's tests. Add to
  the tests by editing ``test/test_project_name.ml``. See :ref:`writing-tests` for
  details on testing.
* The ``lib`` directory will hold the library you write to provide your executable's core
  functionality. Add modules to your library by creating new
  ``.ml`` files in this directory. See :doc:`/reference/dune/library` for
  details on specifying libraries manually.
* The ``bin`` directory holds a skeleton for the executable program. Within the
  modules in this directory, you can access the modules in your ``lib`` under
  the namespace ``project_name.Mod``, where ``project_name`` is replaced with
  the name of your project and ``Mod`` corresponds to the name of the file in
  the ``lib`` directory. You can run the executable with ``dune exec
  project_name``.  See :ref:`hello-world-program` for an example of specifying
  an executable manually and :doc:`/reference/dune/executable` for
  details.
* The ``project_name.opam`` file will be freshly generated from the
  ``dune-project`` file whenever you build your project. You shouldn't need to
  worry about this, but you can see :doc:`explanation/opam-integration` for
  details.
* The ``dune`` files in each directory specify the component to be built with
  the files in that directory. For details on ``dune`` files, see :doc:`/reference/dune/index`.

Initializing a Library
----------------------

To initialize a project for an OCaml library, run the following (replacing
``project_name`` with the name of your project):

.. code:: console

  $ dune init proj --kind=lib project_name

This creates a project directory that includes the following contents:

.. code::

    project_name/
    ├── dune-project
    ├── lib
    │   └── dune
    ├── test
    │   ├── dune
    │   └── test_project_name.ml
    └── project_name.opam

Now, enter your project's directory:

.. code:: console

  $ cd project_name

Then, you can build your project with:

.. code:: console

  $ dune build

You can run your tests with:

.. code:: console

  $ dune test

All of the subcomponents generated are the same as those described in
:ref:`initializing-an-executable`, with the following exceptions:

* There is no ``bin`` directory generated.
* The ``dune`` file in the ``lib`` directory specifies that the library should
  be *public*. See :doc:`/reference/dune/library` for details.

.. _hello-world-program:

Building a Hello World Program From Scratch
===========================================

Create a new directory within a Dune project (:ref:`initializing-an-executable`).
Since OCaml is a compiled language, first create a ``dune`` file in Nano, Vim, 
or your preferred text editor. Declare the ``hello_world`` executable by including the following stanza 
(shown below). Name this initial file ``dune`` and save it. 

.. code:: dune

    (executable
     (name hello_world))

Create a second file containing the following code and name it ``hello_world.ml`` (including 
the .ml extension). It will implement the executable stanza in the ``dune`` file when built. 

.. code:: ocaml

    print_endline "Hello, world!"

Next, build your new program in a shell using this command:

.. code:: console

   $ dune build hello_world.exe

This will create a directory called ``_build`` and build the 
program: ``_build/default/hello_world.exe``. Note that
native code executables will have the ``.exe`` extension on all platforms
(including non-Windows systems). 

Finally, run it with the following command to see that it worked. In 
fact, the executable can both be built and run in a single
step:

.. code:: console

   $ dune exec -- ./hello_world.exe

Voila! This should print "Hello, world!" in the command line.

Building a Hello World Program Using Lwt
========================================

Lwt is a concurrent library in OCaml. 

In a directory of your choice, write this ``dune`` file:

.. code:: dune

    (executable
     (name hello_world)
     (libraries lwt.unix))

This ``hello_world.ml`` file:

.. code:: ocaml

    Lwt_main.run (Lwt_io.printf "Hello, world!\n")

And build it with:

.. code:: console

  $ dune build hello_world.exe

The executable will be built as ``_build/default/hello_world.exe``

Building a Hello World Program Using Core and Jane Street PPXs
==============================================================

Write this ``dune`` file:

.. code:: dune

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

.. code:: console

  $ dune build hello_world.exe

The executable will be built as ``_build/default/hello_world.exe``


Defining a Library Using Lwt and ``ocaml-re``
=============================================

Write this ``dune`` file:

.. code:: dune

    (library
     (name        mylib)
     (public_name mylib)
     (libraries re lwt))

The library will be composed of all the modules in the same directory.
Outside of the library, module ``Foo`` will be accessible as
``Mylib.Foo``, unless you write an explicit ``mylib.ml`` file.

You can then use this library in any other directory by adding ``mylib``
to the ``(libraries ...)`` field.

Building a Hello World Program in Bytecode
============================================

In a directory of your choice, write this ``dune`` file:

.. code:: dune

    ;; This declares the hello_world executable implemented by hello_world.ml
    ;; to be build as native (.exe) or bytecode (.bc) version.
    (executable
     (name hello_world)
     (modes byte exe))

This ``hello_world.ml`` file:

.. code:: ocaml

    print_endline "Hello, world!"

And build it with:

.. code:: console

  $ dune build hello_world.bc

The executable will be built as ``_build/default/hello_world.bc``.
The executable can be built and run in a single
step with ``dune exec ./hello_world.bc``. This bytecode version allows the usage of 
``ocamldebug``.

Setting the OCaml Compilation Flags Globally
============================================

Write this ``dune`` file at the root of your project:

.. code:: dune

    (env
     (dev
      (flags (:standard -w +42)))
     (release
      (ocamlopt_flags (:standard -O3))))

`dev` and `release` correspond to build profiles. The build profile
can be selected from the command line with ``--profile foo`` or from a
`dune-workspace` file by writing:

.. code:: dune

    (profile foo)

Using Cppo
==========

Add this field to your ``library`` or ``executable`` stanzas:

.. code:: dune

    (preprocess (action (run %{bin:cppo} -V OCAML:%{ocaml_version} %{input-file})))

Additionally, if you want to include a ``config.h`` file, you need to
declare the dependency to this file via:

.. code:: dune

    (preprocessor_deps config.h)

Using the ``.cppo.ml`` Style Like the ``ocamlbuild`` Plugin
-----------------------------------------------------------

Write this in your ``dune`` file:

.. code:: dune

    (rule
     (targets foo.ml)
     (deps    (:first-dep foo.cppo.ml) <other files that foo.ml includes>)
     (action  (run %{bin:cppo} %{first-dep} -o %{targets})))

Defining a Library with C Stubs
===============================

Assuming you have a file called ``mystubs.c``, that you need to pass
``-I/blah/include`` to compile it and ``-lblah`` at link time, write
this ``dune`` file:

.. code:: dune

    (library
     (name            mylib)
     (public_name     mylib)
     (libraries       re lwt)
     (foreign_stubs
      (language c)
      (names mystubs)
      (flags -I/blah/include))
     (c_library_flags (-lblah)))

Defining a Library with C Stubs using ``pkg-config``
====================================================

Same context as before, but using ``pkg-config`` to query the
compilation and link flags. Write this ``dune`` file:

.. code:: dune

    (library
     (name            mylib)
     (public_name     mylib)
     (libraries       re lwt)
     (foreign_stubs
      (language c)
      (names mystubs)
      (flags (:include c_flags.sexp)))
     (c_library_flags (:include c_library_flags.sexp)))

    (rule
     (targets c_flags.sexp c_library_flags.sexp)
     (action  (run ./config/discover.exe)))

Then create a ``config`` subdirectory and write this ``dune`` file:

.. code:: dune

    (executable
     (name discover)
     (libraries dune-configurator))

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


Using a Custom Code Generator
=============================

To generate a file ``foo.ml`` using a program from another directory:

.. code:: dune

    (rule
     (targets foo.ml)
     (deps    (:gen ../generator/gen.exe))
     (action  (run %{gen} -o %{targets})))

Defining Tests
==============

Write this in your ``dune`` file:

.. code:: dune

    (test (name my_test_program))

And run the tests with:

.. code:: console

  $ dune runtest

It will run the test program (the main module is ``my_test_program.ml``) and
error if it exits with a nonzero code.

In addition, if a ``my_test_program.expected`` file exists, it will be compared
to the standard output of the test program and the differences will be
displayed. It is possible to replace the ``.expected`` file with the last output
using:

.. code:: console

  $ dune promote

Building a Custom Toplevel
==========================

A toplevel is simply an executable calling ``Topmain.main ()`` and linked with
the compiler libraries and ``-linkall``. Moreover, currently toplevels can only
be built in bytecode.

As a result, write this in your ``dune`` file:

.. code:: dune

    (executable
     (name       mytoplevel)
     (libraries  compiler-libs.toplevel mylib)
     (link_flags (-linkall))
     (modes      byte))

And write this in ``mytoplevel.ml``:

.. code:: ocaml

    let () = exit (Topmain.main ())
