Explore PPX lint correction and promotion for Reason sources under
`melange.emit`.

  $ cat > dune-project <<'EOF'
  > (lang dune 3.13)
  > (using melange 0.1)
  > EOF

  $ mkdir -p ppx shared override

  $ cat > ppx/dune <<'EOF'
  > (library
  >  (name correct_static_add)
  >  (kind ppx_rewriter)
  >  (libraries ppxlib))
  > EOF

  $ cat > ppx/correct_static_add.ml <<'EOF'
  > let detect_static_add = object
  >   inherit Ppxlib.Ast_traverse.iter as super
  > 
  >   method! expression e =
  >     match e with
  >     | { pexp_desc =
  >           Pexp_apply
  >             ( {pexp_desc = Pexp_ident {txt = Lident "+"; _}; _}
  >             , [ (Nolabel, {pexp_desc = Pexp_constant (Pconst_integer (a, None)); _})
  >               ; (Nolabel, {pexp_desc = Pexp_constant (Pconst_integer (b, None)); _})
  >               ]
  >             )
  >       ; pexp_loc = loc
  >       ; _
  >       } ->
  >       let sum = int_of_string a + int_of_string b in
  >       let repl = string_of_int sum in
  >       Ppxlib.Driver.register_correction ~loc ~repl
  >     | _ -> super#expression e
  > end
  > 
  > let impl s =
  >   detect_static_add#structure s; s
  > 
  > let () =
  >   Ppxlib.Driver.register_transformation
  >     "detect_static_add"
  >     ~impl
  > EOF

  $ cat > shared/dune <<'EOF'
  > (melange.emit
  >  (target out)
  >  (emit_stdlib false)
  >  (lint
  >   (pps correct_static_add)))
  > EOF

  $ cat > shared/add.re <<'EOF'
  > let x = 1 + 2;
  > EOF

  $ dune build @shared/lint
  File "shared/add.re.ml", line 1:
  Error: ppxlib_driver: the rewriting contains parts from another file.
  It is too complicated to reconcile it with the source: shared/add.re or shared/add.re and shared/add.re.ml
  [1]
  $ dune promote shared/add.re
  Warning: Nothing to promote for shared/add.re.
