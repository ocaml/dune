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
  > let compilation_mode = ref "absent"
  > let () =
  >   Driver.Cookies.add_handler (fun cookies ->
  >     message := Option.value ~default:"missing"
  >       (Driver.Cookies.get cookies "message" Ast_pattern.(estring __));
  >     compilation_mode := Option.value ~default:"absent"
  >       (Driver.Cookies.get cookies "compilation-mode"
  >          Ast_pattern.(estring __)))
  > let extension name value =
  >   Extension.declare name Expression Ast_pattern.__ (fun ~loc ~path:_ _ ->
  >     Ast_builder.Default.estring ~loc !value)
  > let () = Driver.register_transformation "message"
  >   ~rules:
  >     [ Context_free.Rule.extension (extension "message" message)
  >     ; Context_free.Rule.extension
  >         (extension "compilation_mode" compilation_mode)
  >     ]
  > EOF
  $ cat >init.ml <<'EOF'
  > print_endline [%message]
  > EOF

Both toplevels preserve the cookie's string value.

  $ dune exec ./tt.exe -- init.ml
  hello
  $ dune utop . -- init.ml
  hello

Dune does not supply the compilation-mode cookie before language version 3.26.

  $ cat >init.ml <<'EOF'
  > print_endline [%compilation_mode]
  > EOF
  $ dune exec ./tt.exe -- init.ml
  absent
  $ dune utop . -- init.ml
  absent

Changing the language version supplies the cookie to both runtime PPX commands,
without declaring it in the PPX library.

  $ make_dune_project 3.26
  $ dune exec ./tt.exe -- init.ml
  ocaml
  $ dune utop . -- init.ml
  ocaml
