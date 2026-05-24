Melange Reason sources are converted by refmt before PPX preprocessing. PPX
locations should still resolve to the original Reason source so compiler
diagnostics can print the correct snippet.

  $ mkdir -p cases/ppx cases/src

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
  > let impl _ =
  >   let loc_start =
  >     { Lexing.pos_fname = !Ocaml_common.Location.input_name
  >     ; pos_lnum = 1
  >     ; pos_bol = 0
  >     ; pos_cnum = 8
  >     }
  >   in
  >   let loc = { Location.loc_start; loc_end = { loc_start with pos_cnum = 12 }; loc_ghost = false } in
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
  > let () = Driver.register_transformation "ppx_locations" ~impl
  > EOF

  $ cat > cases/src/dune <<'EOF'
  > (library
  >  (name logical_reason_impl)
  >  (modes melange)
  >  (modules x)
  >  (preprocess
  >   (pps ppx_locations)))
  > EOF

  $ cat > cases/src/x.re <<'EOF'
  > let x = "reason"
  > EOF

  $ dune build --root cases --sandbox=symlink @src/all
  Entering directory 'cases'
  File "src/x.re", line 1, characters 8-12:
  1 | let x = "reason"
              ^^^^
  Error: Unbound type constructor nope
  Leaving directory 'cases'
  [1]
