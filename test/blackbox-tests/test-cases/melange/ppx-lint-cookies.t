PPX lint drivers receive cookies declared by their libraries for both
implementations and interfaces, in OCaml and Melange libraries.

  $ make_melange_project 3.25 1.0
  $ mkdir ppx
  $ cat >ppx/dune <<'EOF'
  > (library
  >  (name cookie_ppx)
  >  (kind (ppx_rewriter (cookies (lint-message "a \"quoted\" cookie"))))
  >  (libraries ppxlib))
  > EOF
  $ cat >ppx/cookie_ppx.ml <<'EOF'
  > open Ppxlib
  > let message = ref "missing cookie"
  > let () =
  >   Driver.Cookies.add_handler (fun cookies ->
  >     message :=
  >       Option.value ~default:"missing cookie"
  >         (Driver.Cookies.get cookies "lint-message"
  >            Ast_pattern.(estring __)))
  > let lint kind _ =
  >   Printf.eprintf "%s: %s\n" kind !message;
  >   []
  > let () =
  >   Driver.register_transformation "cookie"
  >     ~lint_impl:(lint "implementation") ~lint_intf:(lint "interface")
  > EOF

  $ mkdir ocaml
  $ cat >ocaml/dune <<'EOF'
  > (library
  >  (name linted_ocaml)
  >  (lint (pps cookie_ppx)))
  > EOF
  $ touch ocaml/linted_ocaml.ml ocaml/linted_ocaml.mli
  $ dune build @ocaml/lint 2>&1 | sort
  implementation: a "quoted" cookie
  interface: a "quoted" cookie

  $ mkdir melange
  $ cat >melange/dune <<'EOF'
  > (library
  >  (name linted_melange)
  >  (modes melange)
  >  (lint (pps cookie_ppx)))
  > EOF
  $ touch melange/linted_melange.ml melange/linted_melange.mli
  $ dune build @melange/lint 2>&1 | sort
  implementation: a "quoted" cookie
  interface: a "quoted" cookie
