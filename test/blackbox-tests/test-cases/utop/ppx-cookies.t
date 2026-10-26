Custom toplevels and UTop preserve string-valued cookies when running PPX
rewriters at runtime.

  $ make_dune_project 3.25
  $ cat >dune <<'EOF'
  > (library
  >  (name cookie_ppx)
  >  (modules cookie_ppx)
  >  (libraries ppxlib)
  >  (kind (ppx_rewriter (cookies (message "hello")))))
  > (toplevel
  >  (name tt)
  >  (preprocess (pps cookie_ppx)))
  > EOF
  $ cat >cookie_ppx.ml <<'EOF'
  > open Ppxlib
  > let message = ref "missing"
  > let () =
  >   Driver.Cookies.add_handler (fun cookies ->
  >     message := Option.value ~default:"missing"
  >       (Driver.Cookies.get cookies "message" Ast_pattern.(estring __)))
  > let extension =
  >   Extension.declare "message" Expression Ast_pattern.__ (fun ~loc ~path:_ _ ->
  >     Ast_builder.Default.estring ~loc !message)
  > let () = Driver.register_transformation "message"
  >   ~rules:[Context_free.Rule.extension extension]
  > EOF
  $ cat >init.ml <<'EOF'
  > print_endline [%message]
  > EOF

Both toplevels preserve the cookie's string value.

  $ dune exec ./tt.exe -- init.ml
  hello
  $ dune utop . -- init.ml
  hello
