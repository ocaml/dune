`dune describe workspace` and Melange targets
=============================================

`melange.emit` stanzas are described as `melange.emit` items, and libraries are
described once per compilation mode they are actually built in: as a `library`
item for OCaml and as a `melange.library` item for Melange. A library built in
both modes therefore appears twice, under two distinct uids.

The `--sanitize-for-tests` flag is required so that absolute paths stay stable
across machines, and `censor` replaces library digests with stable labels.

A single melange.emit stanza
----------------------------

The emit is described by a `melange.emit` item covering `dist` and the `main`
module.

  $ mkdir single && cd single
  $ make_melange_project 3.8 0.1
  $ cat > dune <<EOF
  > (melange.emit
  >  (target dist)
  >  (emit_stdlib false))
  > EOF
  $ cat > main.ml <<EOF
  > let () = Js.log "hello"
  > EOF
  $ dune describe workspace --lang 0.1 --sanitize-for-tests | censor
  ((root /WORKSPACE_ROOT)
   (build_context _build/default)
   (melange.emit
    ((target dist)
     (alias melange)
     (module_systems ((commonjs .js)))
     (target_dir _build/default/dist)
     (requires ())
     (modules
      (((name Main)
        (impl (_build/default/.melange_src/main.ml))
        (intf ())
        (cmt (_build/default/.dist.mobjs/byte/melange__Main.cmt))
        (cmti ())
        (origin (dune ((dune_file dune)))))))
     (include_dirs (_build/default/.dist.mobjs/byte)))))
  $ cd ..

An emit depending on a Melange-only library
-------------------------------------------

The library is built for Melange only, so it is described by a single
`melange.library` item. It no longer produces a `library` item, which used to
report OCaml artifacts that are never built.

  $ mkdir mel-lib && cd mel-lib
  $ make_melange_project 3.8 0.1
  $ mkdir lib
  $ cat > lib/dune <<EOF
  > (library
  >  (name mylib)
  >  (modes melange))
  > EOF
  $ cat > lib/mylib.ml <<EOF
  > let x = "hello"
  > EOF
  $ cat > dune <<EOF
  > (melange.emit
  >  (target dist)
  >  (emit_stdlib false)
  >  (libraries mylib))
  > EOF
  $ cat > main.ml <<EOF
  > let () = Js.log Mylib.x
  > EOF
  $ dune describe workspace --lang 0.1 --sanitize-for-tests | censor
  ((root /WORKSPACE_ROOT)
   (build_context _build/default)
   (melange.emit
    ((target dist)
     (alias melange)
     (module_systems ((commonjs .js)))
     (target_dir _build/default/dist)
     (requires ($DIGEST))
     (modules
      (((name Main)
        (impl (_build/default/.melange_src/main.ml))
        (intf ())
        (cmt (_build/default/.dist.mobjs/byte/melange__Main.cmt))
        (cmti ())
        (origin (dune ((dune_file dune)))))))
     (include_dirs (_build/default/.dist.mobjs/byte))))
   (melange.library
    ((name mylib)
     (uid $DIGEST)
     (local true)
     (requires ())
     (source_dir _build/default/lib)
     (modules
      (((name Mylib)
        (impl (_build/default/lib/.melange_src/mylib.ml))
        (intf ())
        (cmt (_build/default/lib/.mylib.objs/byte/mylib.cmt))
        (cmti ())
        (origin (dune ((dune_file lib/dune)))))))
     (include_dirs (_build/default/lib/.mylib.objs/byte)))))
  $ cd ..

A library used by both an executable and a melange.emit
-------------------------------------------------------

The library is built in both OCaml and Melange mode, so it is described twice,
by a `library` item and a `melange.library` item with distinct uids. The
executable's `requires` names the OCaml uid and the emit's `requires` names the
Melange one.

  $ mkdir shared && cd shared
  $ make_melange_project 3.8 0.1
  $ mkdir lib
  $ cat > lib/dune <<EOF
  > (library
  >  (name shared_lib)
  >  (modes melange :standard))
  > EOF
  $ cat > lib/shared_lib.ml <<EOF
  > let x = "shared"
  > EOF
  $ mkdir exe
  $ cat > exe/dune <<EOF
  > (executable
  >  (name main)
  >  (libraries shared_lib))
  > EOF
  $ cat > exe/main.ml <<EOF
  > let () = print_endline Shared_lib.x
  > EOF
  $ mkdir emit
  $ cat > emit/dune <<EOF
  > (melange.emit
  >  (target dist)
  >  (emit_stdlib false)
  >  (libraries shared_lib))
  > EOF
  $ cat > emit/app.ml <<EOF
  > let () = Js.log Shared_lib.x
  > EOF
  $ dune describe workspace --lang 0.1 --sanitize-for-tests | censor
  ((root /WORKSPACE_ROOT)
   (build_context _build/default)
   (executables
    ((names (main))
     (requires ($DIGEST1))
     (modules
      (((name Main)
        (impl (_build/default/exe/main.ml))
        (intf ())
        (cmt (_build/default/exe/.main.eobjs/byte/dune__exe__Main.cmt))
        (cmti ())
        (origin source))))
     (include_dirs (_build/default/exe/.main.eobjs/byte))))
   (melange.emit
    ((target dist)
     (alias melange)
     (module_systems ((commonjs .js)))
     (target_dir _build/default/emit/dist)
     (requires ($DIGEST2))
     (modules
      (((name App)
        (impl (_build/default/emit/.melange_src/app.ml))
        (intf ())
        (cmt (_build/default/emit/.dist.mobjs/byte/melange__App.cmt))
        (cmti ())
        (origin (dune ((dune_file emit/dune)))))))
     (include_dirs (_build/default/emit/.dist.mobjs/byte))))
   (library
    ((name shared_lib)
     (uid $DIGEST1)
     (local true)
     (requires ())
     (source_dir _build/default/lib)
     (modules
      (((name Shared_lib)
        (impl (_build/default/lib/shared_lib.ml))
        (intf ())
        (cmt (_build/default/lib/.shared_lib.objs/byte/shared_lib.cmt))
        (cmti ())
        (origin source))))
     (include_dirs (_build/default/lib/.shared_lib.objs/byte))))
   (melange.library
    ((name shared_lib)
     (uid $DIGEST2)
     (local true)
     (requires ())
     (source_dir _build/default/lib)
     (modules
      (((name Shared_lib)
        (impl (_build/default/lib/.melange_src/shared_lib.ml))
        (intf ())
        (cmt (_build/default/lib/.shared_lib.objs/byte/shared_lib.cmt))
        (cmti ())
        (origin (dune ((dune_file lib/dune)))))))
     (include_dirs (_build/default/lib/.shared_lib.objs/byte)))))
  $ cd ..

An OCaml-only library in a project that also has an emit
--------------------------------------------------------

This library is not usable from Melange, so it is described by exactly one
`library` item and no `melange.library` item: it guards the filter based on
`Lib_info.effective_modes`.

  $ mkdir ocaml-only && cd ocaml-only
  $ make_melange_project 3.8 0.1
  $ mkdir lib
  $ cat > lib/dune <<EOF
  > (library
  >  (name ocaml_only))
  > EOF
  $ cat > lib/ocaml_only.ml <<EOF
  > let x = "ocaml only"
  > EOF
  $ mkdir emit
  $ cat > emit/dune <<EOF
  > (melange.emit
  >  (target dist)
  >  (emit_stdlib false))
  > EOF
  $ cat > emit/app.ml <<EOF
  > let () = Js.log "app"
  > EOF
  $ dune describe workspace --lang 0.1 --sanitize-for-tests | censor
  ((root /WORKSPACE_ROOT)
   (build_context _build/default)
   (melange.emit
    ((target dist)
     (alias melange)
     (module_systems ((commonjs .js)))
     (target_dir _build/default/emit/dist)
     (requires ())
     (modules
      (((name App)
        (impl (_build/default/emit/.melange_src/app.ml))
        (intf ())
        (cmt (_build/default/emit/.dist.mobjs/byte/melange__App.cmt))
        (cmti ())
        (origin (dune ((dune_file emit/dune)))))))
     (include_dirs (_build/default/emit/.dist.mobjs/byte))))
   (library
    ((name ocaml_only)
     (uid $DIGEST)
     (local true)
     (requires ())
     (source_dir _build/default/lib)
     (modules
      (((name Ocaml_only)
        (impl (_build/default/lib/ocaml_only.ml))
        (intf ())
        (cmt (_build/default/lib/.ocaml_only.objs/byte/ocaml_only.cmt))
        (cmti ())
        (origin source))))
     (include_dirs (_build/default/lib/.ocaml_only.objs/byte)))))
  $ cd ..

The csexp format
----------------

The csexp output is a single long line, so it is written to a file and
pretty-printed back before being compared. This guards the printing of the
dotted `melange.emit` and `melange.library` atoms.

  $ mkdir csexp && cd csexp
  $ make_melange_project 3.8 0.1
  $ mkdir lib
  $ cat > lib/dune <<EOF
  > (library
  >  (name mylib)
  >  (modes melange))
  > EOF
  $ cat > lib/mylib.ml <<EOF
  > let x = "hello"
  > EOF
  $ cat > dune <<EOF
  > (melange.emit
  >  (target dist)
  >  (emit_stdlib false)
  >  (libraries mylib))
  > EOF
  $ cat > main.ml <<EOF
  > let () = Js.log Mylib.x
  > EOF
  $ dune describe workspace --format csexp --lang 0.1 --sanitize-for-tests \
  >   > workspace.csexp
  $ dune internal sexp-pp --format csexp workspace.csexp | censor
  ((root /WORKSPACE_ROOT)
   (build_context _build/default)
   (melange.emit
    ((target dist)
     (alias melange)
     (module_systems
      ((commonjs .js)))
     (target_dir _build/default/dist)
     (requires ($DIGEST))
     (modules
      (((name Main)
        (impl (_build/default/.melange_src/main.ml))
        (intf ())
        (cmt (_build/default/.dist.mobjs/byte/melange__Main.cmt))
        (cmti ())
        (origin
         (dune
          ((dune_file dune)))))))
     (include_dirs (_build/default/.dist.mobjs/byte))))
   (melange.library
    ((name mylib)
     (uid $DIGEST)
     (local true)
     (requires ())
     (source_dir _build/default/lib)
     (modules
      (((name Mylib)
        (impl (_build/default/lib/.melange_src/mylib.ml))
        (intf ())
        (cmt (_build/default/lib/.mylib.objs/byte/mylib.cmt))
        (cmti ())
        (origin
         (dune
          ((dune_file lib/dune)))))))
     (include_dirs (_build/default/lib/.mylib.objs/byte)))))
  $ cd ..
