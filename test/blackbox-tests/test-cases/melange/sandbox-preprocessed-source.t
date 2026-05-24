Melange copies OCaml source files into `.melange_src` before preprocessing.
For preprocessed `.ml` and `.mli` files, compiler diagnostics may need either
the copied pre-PPX source, for locations that still point at `.melange_src`, or
the original source, for ppxlib-generated locations that use the logical input
filename.

  $ mkdir -p cases/ppx

  $ cat > cases/dune-project <<'EOF'
  > (lang dune 3.18)
  > (using melange 0.1)
  > EOF

  $ cat > cases/ppx/dune <<'EOF'
  > (library
  >  (name ppx_locations)
  >  (kind ppx_rewriter)
  >  (libraries ppxlib))
  > EOF

  $ cat > cases/ppx/ppx_locations.ml <<'EOF'
  > open Ppxlib
  > 
  > let has_substring s substr =
  >   let len = String.length s in
  >   let substr_len = String.length substr in
  >   let rec loop i =
  >     i + substr_len <= len
  >     && (String.sub s i substr_len = substr || loop (i + 1))
  >   in
  >   loop 0
  > ;;
  > 
  > let input_path suffix =
  >   Array.find_opt
  >     (fun arg -> has_substring arg (".melange_src/x" ^ suffix))
  >     Sys.argv
  >   |> Option.get
  > ;;
  > 
  > let loc ~suffix =
  >   ignore (input_path suffix);
  >   let loc_start =
  >     { Lexing.pos_fname = !Ocaml_common.Location.input_name
  >     ; pos_lnum = 1
  >     ; pos_bol = 0
  >     ; pos_cnum = 8
  >     }
  >   in
  >   let loc_end = { loc_start with pos_cnum = 12 } in
  >   { Location.loc_start; loc_end; loc_ghost = false }
  > ;;
  > 
  > let impl _ =
  >   let loc = loc ~suffix:".ml" in
  >   let open Ast_builder.Default in
  >   [ pstr_value
  >       ~loc
  >       Nonrecursive
  >       [ value_binding
  >           ~loc
  >           ~pat:(ppat_var ~loc { txt = "x"; loc })
  >           ~expr:
  >             (pexp_constraint
  >                ~loc
  >                (eunit ~loc)
  >                (ptyp_constr ~loc { txt = Longident.Lident "nope"; loc } []))
  >       ]
  >   ]
  > ;;
  > 
  > let intf _ =
  >   let loc = loc ~suffix:".mli" in
  >   let open Ast_builder.Default in
  >   [ psig_value
  >       ~loc
  >       (value_description
  >          ~loc
  >          ~name:{ txt = "x"; loc }
  >          ~type_:(ptyp_constr ~loc { txt = Longident.Lident "nope"; loc } [])
  >          ~prim:[])
  >   ]
  > ;;
  > 
  > let () = Driver.register_transformation "ppx_locations" ~impl ~intf
  > EOF

Action preprocessors may emit locations that still point at a copied Melange
implementation. The copied source must be available in the compile sandbox.

  $ mkdir -p cases/copied-impl/src

  $ cat > cases/copied-impl/src/dune <<'EOF'
  > (library
  >  (name copied_impl)
  >  (modes melange)
  >  (modules x)
  >  (preprocess
  >   (action
  >    (run sh -c "printf '# 1 \"%s\"\\nlet x : nope = ()\\n' \"$1\"" -- %{input-file}))))
  > EOF

  $ cat > cases/copied-impl/src/x.ml <<'EOF'
  > let x = 1
  > EOF

  $ dune build --root cases --sandbox=symlink @copied-impl/src/all
  Entering directory 'cases'
  File "copied-impl/src/.melange_src/x.ml", line 1, characters 8-12:
  Error: Unbound type constructor nope
  Leaving directory 'cases'
  [1]

ppxlib-generated implementation locations should use the original source
filename even though the PPX reads the copied Melange source.

  $ mkdir -p cases/logical-ocaml-impl/src

  $ cat > cases/logical-ocaml-impl/src/dune <<'EOF'
  > (library
  >  (name logical_ocaml_impl)
  >  (modes melange)
  >  (modules x)
  >  (preprocess
  >   (pps ppx_locations)))
  > EOF

  $ cat > cases/logical-ocaml-impl/src/x.ml <<'EOF'
  > let x = "ocaml"
  > EOF

  $ dune build --root cases --sandbox=symlink @logical-ocaml-impl/src/all
  Entering directory 'cases'
  File "logical-ocaml-impl/src/.melange_src/x.ml", line 1, characters 8-12:
  Error: Unbound type constructor nope
  Leaving directory 'cases'
  [1]

ppxlib-generated implementation locations should use the original conditional
Melange source filename even though the PPX reads the copied Melange source.

  $ mkdir -p cases/logical-melange-impl/src

  $ cat > cases/logical-melange-impl/src/dune <<'EOF'
  > (library
  >  (name logical_melange_impl)
  >  (modes melange)
  >  (modules x)
  >  (preprocess
  >   (pps ppx_locations)))
  > EOF

  $ cat > cases/logical-melange-impl/src/x.ml <<'EOF'
  > let x = "ocaml"
  > EOF

  $ cat > cases/logical-melange-impl/src/x.melange.ml <<'EOF'
  > let x = "melange"
  > EOF

  $ dune build --root cases --sandbox=symlink @logical-melange-impl/src/all
  Entering directory 'cases'
  File "logical-melange-impl/src/.melange_src/x.ml", line 1, characters 8-12:
  Error: Unbound type constructor nope
  Leaving directory 'cases'
  [1]

ppxlib-generated interface locations should also use the original source
filename even though the PPX reads the copied Melange source.

  $ mkdir -p cases/logical-ocaml-intf/src

  $ cat > cases/logical-ocaml-intf/src/dune <<'EOF'
  > (library
  >  (name logical_ocaml_intf)
  >  (modes melange)
  >  (modules x)
  >  (preprocess
  >   (pps ppx_locations)))
  > EOF

  $ cat > cases/logical-ocaml-intf/src/x.mli <<'EOF'
  > val x : int
  > EOF

  $ cat > cases/logical-ocaml-intf/src/x.ml <<'EOF'
  > let x = Obj.magic ()
  > EOF

  $ dune build --root cases --sandbox=symlink @logical-ocaml-intf/src/all
  Entering directory 'cases'
  File "logical-ocaml-intf/src/.melange_src/x.mli", line 1, characters 8-12:
  Error: Unbound type constructor nope
  Leaving directory 'cases'
  [1]

ppxlib-generated interface locations should use the original conditional
Melange source filename even though the PPX reads the copied Melange source.

  $ mkdir -p cases/logical-melange-intf/src

  $ cat > cases/logical-melange-intf/src/dune <<'EOF'
  > (library
  >  (name logical_melange_intf)
  >  (modes melange)
  >  (modules x)
  >  (preprocess
  >   (pps ppx_locations)))
  > EOF

  $ cat > cases/logical-melange-intf/src/x.mli <<'EOF'
  > val x : int
  > EOF

  $ cat > cases/logical-melange-intf/src/x.melange.mli <<'EOF'
  > val x : string
  > EOF

  $ cat > cases/logical-melange-intf/src/x.ml <<'EOF'
  > let x = Obj.magic ()
  > EOF

  $ dune build --root cases --sandbox=symlink @logical-melange-intf/src/all
  Entering directory 'cases'
  File "logical-melange-intf/src/.melange_src/x.mli", line 1, characters 8-12:
  Error: Unbound type constructor nope
  Leaving directory 'cases'
  [1]
