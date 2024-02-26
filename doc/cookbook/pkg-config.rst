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
