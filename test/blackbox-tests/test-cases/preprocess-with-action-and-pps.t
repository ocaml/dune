Preprocess with an action chain and PPX

  $ cat > dune-project <<EOF
  > (lang dune 3.8)
  > EOF
  $ cat > dune <<EOF
  > (library
  >  (name someppx)
  >  (modules someppx)
  >  (libraries ppxlib)
  >  (kind ppx_rewriter))
  > (executable
  >  (name foo)
  >  (modules foo)
  >  (preprocess
  >   (action
  >    (progn
  >      (with-stdout-to %{input-file}.pp1
  >        (progn
  >         (run cat %{input-file})
  >         (echo "let () = print_endline \"one more line\"")))
  >      (ppx (someppx) %{input-file}.pp1)))))
  > EOF
  $ cat > foo.ml <<EOF
  > let () = print_endline "replaced with forty-two:"
  > let () = print_endline (string_of_int [%replace_with_42 1])
  > EOF

  $ cat >someppx.ml <<EOF
  > open Ppxlib
  > let expand ~ctxt _i =
  >   let loc = Expansion_context.Extension.extension_point_loc ctxt in
  >   Ast_builder.Default.eint ~loc 42
  > let my_extension =
  >   Extension.V3.declare "replace_with_42" Extension.Context.expression
  >     Ast_pattern.(single_expr_payload (eint __))
  >     expand
  > let rule = Ppxlib.Context_free.Rule.extension my_extension
  > let () = Driver.register_transformation ~rules:[ rule ] "replace_with_42"
  > EOF

  $ dune exec ./foo.exe
  replaced with forty-two:
  42
  one more line

  $ ls _build/default
  foo.exe
  foo.ml
  foo.ml.pp1
  foo.pp.ml
  foo.pp.mli
  someppx.a
  someppx.cmxa
  someppx.ml

Merlin support is currently missing from this feature

  $ dune ocaml merlin dump-config "$PWD" | grep "\-pp"
  [1]
