`dune describe workspace` and Melange targets
=============================================

`dune describe workspace` only ever describes the OCaml compilation of a
workspace. Melange targets are therefore invisible today: `melange.emit`
stanzas are not described at all, and every library is described exactly once,
in OCaml mode, even when it is only ever built for Melange. This test records
that current behaviour; a later change will make these outputs contain
`melange.emit` and `melange.library` items.

The `--sanitize-for-tests` flag is required so that absolute paths stay stable
across machines, and `censor` replaces library digests with stable labels.

A single melange.emit stanza
----------------------------

Nothing is printed for the emit or its modules. Later there should be a
`melange.emit` item describing `dist` and the `main` module.

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
   (build_context _build/default))
  $ cd ..

An emit depending on a Melange-only library
-------------------------------------------

The library is built for Melange only, yet it is described as an ordinary
`library` item, with OCaml `.objs/byte` artifacts that are never built. Later
it should be described as a `melange.library` item instead.

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
   (library
    ((name mylib)
     (uid $DIGEST)
     (local true)
     (requires ())
     (source_dir _build/default/lib)
     (modules
      (((name Mylib)
        (impl (_build/default/lib/mylib.ml))
        (intf ())
        (cmt (_build/default/lib/.mylib.objs/byte/mylib.cmt))
        (cmti ())
        (origin source))))
     (include_dirs (_build/default/lib/.mylib.objs/byte)))))
  $ cd ..

A library used by both an executable and a melange.emit
-------------------------------------------------------

The library is built in both OCaml and Melange mode, but there is exactly one
`library` item for it. Later there should be two items with distinct uids: one
`library` and one `melange.library`.

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
     (requires ($DIGEST))
     (modules
      (((name Main)
        (impl (_build/default/exe/main.ml))
        (intf ())
        (cmt (_build/default/exe/.main.eobjs/byte/dune__exe__Main.cmt))
        (cmti ())
        (origin source))))
     (include_dirs (_build/default/exe/.main.eobjs/byte))))
   (library
    ((name shared_lib)
     (uid $DIGEST)
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
     (include_dirs (_build/default/lib/.shared_lib.objs/byte)))))
  $ cd ..

An OCaml-only library in a project that also has an emit
--------------------------------------------------------

This library is not usable from Melange, so it must keep exactly one `library`
item even after Melange targets are described: it guards a filter based on
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
dotted `melange.emit` and `melange.library` atoms that a later change
introduces.

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
   (library
    ((name mylib)
     (uid $DIGEST)
     (local true)
     (requires ())
     (source_dir _build/default/lib)
     (modules
      (((name Mylib)
        (impl (_build/default/lib/mylib.ml))
        (intf ())
        (cmt (_build/default/lib/.mylib.objs/byte/mylib.cmt))
        (cmti ())
        (origin source))))
     (include_dirs (_build/default/lib/.mylib.objs/byte)))))
  $ cd ..
