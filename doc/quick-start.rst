**********
Quickstart
**********

This document gives simple usage examples of Jbuilder. You can also look at
`examples <https://github.com/janestreet/jbuilder/tree/master/example>`__ for
complete examples of projects using Jbuilder.

Building a hello world program
==============================

In a directory of your choice, write this ``jbuild`` file:

.. code:: scheme

    (jbuild_version 1)

    (executables
     ((names (hello_world))))

This ``hello_world.ml`` file:

.. code:: ocaml

    print_endline "Hello, world!"

And build it with:

.. code:: bash

    jbuilder build hello_world.exe

The executable will be built as ``_build/default/hello_world.exe``

Building a hello world program using Lwt
========================================

In a directory of your choice, write this ``jbuild`` file:

.. code:: scheme

    (jbuild_version 1)

    (executables
     ((names (hello_world))
      (libraries (lwt.unix))))

This ``hello_world.ml`` file:

.. code:: scheme

    Lwt_main.run (Lwt_io.printf "Hello, world!\n")

And build it with:

.. code:: bash

    jbuilder build hello_world.exe

The executable will be built as ``_build/default/hello_world.exe``

Defining a library using Lwt and ocaml-re
=========================================

Write this jbuild:

.. code:: scheme

    (jbuild_version 1)

    (library
     ((name        mylib)
      (public_name mylib)
      (libraries (re lwt))))

The library will be composed of all the modules in the same directory.
Outside of the library, module ``Foo`` will be accessible as
``Mylib.Foo``, unless you write an explicit ``mylib.ml`` file.

You can them use this library in any other directory by adding ``mylib``
to the ``(libraries ...)`` field.

Using cppo
==========

Add this field to your ``library`` or ``executables`` stanzas:

.. code:: scheme

    (preprocess (action (run ${bin:cppo} -V OCAML:${ocaml_version} ${<})))

Additionnaly, if you are include a ``config.h`` file, you need to
declare the dependency to this file via:

.. code:: scheme

    (preprocessor_deps (config.h))

Using the .cppo.ml style like the ocamlbuild plugin
---------------------------------------------------

Write this in your jbuild:

.. code:: scheme

    (rule
     ((targets (foo.ml))
      (deps    (foo.cppo.ml <other files that foo.ml includes>))
      (action  (run ${bin:cppo} ${<} -o ${@}))))

Defining a library with C stubs
===============================

Assuming you have a file called ``mystubs.c``, that you need to pass
``-I/blah/include`` to compile it and ``-lblah`` at link time, write
this jbuild:

.. code:: scheme

    (jbuild_version 1)

    (library
     ((name            mylib)
      (public_name     mylib)
      (libraries       (re lwt))
      (c_names         (mystubs)
      (c_flags         (-I/blah/include))
      (c_library_flags (-lblah)))))

Defining a library with C stubs using pkg-config
================================================

Same context as before, but using ``pkg-config`` to query the
compilation and link flags. Write this jbuild:

.. code:: scheme

    (jbuild_version 1)

    (library
     ((name            mylib)
      (public_name     mylib)
      (libraries       (re lwt))
      (c_names         (mystubs)
      (c_flags         (:include c_flags.sexp))
      (c_library_flags (:include c_library_flags.sexp)))))

    (rule
     ((targets (c_flags.sexp
                c_library_flags.sexp))
      (deps    (config/discover.exe))
      (action  (run ${<} -ocamlc ${OCAMLC}))))

Then create a ``config`` subdirectory and write this ``jbuild``:

.. code:: scheme

    (jbuild_version 1)

    (executables
     ((names (discover))
      (libraries (base stdio configurator))))

as well as this ``discover.ml`` file:

.. code:: ocaml

    open Base
    open Stdio
    module C = Configurator

    let write_sexp fn sexp =
      Out_channel.write_all fn ~data:(Sexp.to_string sexp)

    let () =
      C.main ~name:"mylib" (fun c ->
        let default : C.Pkg_config.package_conf =
          { libs   = ["-lblah"]
          ; cflags = []
          }
        in
        let conf =
          match C.Pkg_config.get c with
          | None -> default
          | Some pc ->
            Option.value (C.Pkg_config.query pc ~package:"blah") ~default
        in

        write_sexp "c_flags.sexp"         (sexp_of_list sexp_of_string conf.libs);
        write_sexp "c_library_flags.sexp" (sexp_of_list sexp_of_string conf.cflags))

Using a custom code generator
=============================

To generate a file ``foo.ml`` using a program from another directory:

.. code:: scheme

    (jbuild_version 1)

    (rule
     ((targets (foo.ml))
      (deps    (../generator/gen.exe))
      (action  (run ${<} -o ${@}))))

Defining tests
==============

Write this in your ``jbuild`` file:

.. code:: scheme

    (jbuild_version 1)

    (alias
     ((name    runtest)
      (deps    (my-test-program.exe))
      (action  (run ${<}))))

And run the tests with:

.. code:: bash

    jbuilder runtest
