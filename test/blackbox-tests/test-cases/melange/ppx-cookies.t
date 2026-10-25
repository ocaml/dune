PPX libraries can declare string cookies. The driver must preserve the value
when preprocessing OCaml and Melange sources with either pps or staged_pps.

  $ make_melange_project 3.25 1.0
  $ mkdir ppx
  $ cat >ppx/dune <<'EOF'
  > (library
  >  (name cookie_ppx)
  >  (kind (ppx_rewriter (cookies (test-message "a \"quoted\" cookie"))))
  >  (libraries ppxlib))
  > EOF
  $ cat >ppx/cookie_ppx.ml <<'EOF'
  > open Ppxlib
  > let message = ref "missing cookie"
  > let () =
  >   Driver.Cookies.add_handler (fun cookies ->
  >     message :=
  >       Option.value ~default:"missing cookie"
  >         (Driver.Cookies.get cookies "test-message"
  >            Ast_pattern.(estring __)))
  > let extension =
  >   Extension.V3.declare "cookie_message" Extension.Context.expression
  >     Ast_pattern.(pstr nil)
  >     (fun ~ctxt ->
  >       let loc = Expansion_context.Extension.extension_point_loc ctxt in
  >       Ast_builder.Default.estring ~loc !message)
  > let () =
  >   Driver.register_transformation "cookie"
  >     ~rules:[Context_free.Rule.extension extension]
  > EOF

The same library is compiled for both targets. Its PPX expands the cookie to
a string literal, without introducing module dependencies.

  $ make_case () {
  >   mkdir "$1"
  >   cat >"$1/dune" <<EOF
  > (library
  >  (name $1_shared)
  >  (wrapped false)
  >  (modules shared)
  >  (modes :standard melange)
  >  (preprocess ($2 cookie_ppx)))
  > (executable
  >  (name main)
  >  (modules main)
  >  (libraries $1_shared))
  > (melange.emit
  >  (target out)
  >  (modules main)
  >  (libraries $1_shared))
  > EOF
  >   echo 'let message = [%cookie_message]' >"$1/shared.ml"
  >   echo 'let () = print_endline Shared.message' >"$1/main.ml"
  > }
  $ make_case ordinary pps
  $ make_case staged staged_pps
  $ dune build ordinary/main.exe @ordinary/melange \
  >   staged/main.exe @staged/melange
  $ _build/default/ordinary/main.exe
  a "quoted" cookie
  $ node _build/default/ordinary/out/ordinary/main.js
  a "quoted" cookie
  $ _build/default/staged/main.exe
  a "quoted" cookie
  $ node _build/default/staged/out/staged/main.js
  a "quoted" cookie

A melange.emit stanza can also preprocess its own modules with the cookie.

  $ mkdir emit
  $ cat >emit/dune <<'EOF'
  > (melange.emit
  >  (target out)
  >  (preprocess (pps cookie_ppx)))
  > EOF
  $ echo 'let () = print_endline [%cookie_message]' >emit/main.ml
  $ dune build @emit/melange
  $ node _build/default/emit/out/emit/main.js
  a "quoted" cookie
