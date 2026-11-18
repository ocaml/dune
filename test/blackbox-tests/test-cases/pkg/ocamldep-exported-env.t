A staged PPX rewrites chosen_value to A.x or B.x according to a package
export. Inspect the consumer's dependencies without compiling it.

  $ make_dune_project 3.25
  $ make_lockdir
  $ mkdir driver lib
  $ cat >driver/dune <<'EOF'
  > (library
  >  (name pick)
  >  (kind ppx_rewriter)
  >  (libraries compiler-libs.common)
  >  (ppx.driver (main Pick.main)))
  > EOF
  $ cat >driver/pick.ml <<'EOF'
  > let main () =
  >   let expr self expr =
  >     match expr.Parsetree.pexp_desc with
  >     | Pexp_ident { txt = Lident "chosen_value"; _ } ->
  >       Parse.expression (Lexing.from_string (Sys.getenv "DEP_MODULE" ^ ".x"))
  >     | _ -> Ast_mapper.default_mapper.expr self expr
  >   in
  >   Ast_mapper.run_main (fun _ -> { Ast_mapper.default_mapper with expr })
  > EOF
  $ cat >lib/dune <<'EOF'
  > (library
  >  (name consumer_lib)
  >  (wrapped false)
  >  (preprocess (staged_pps pick)))
  > EOF
  $ echo 'let x = 1' >lib/a.ml
  $ echo 'let x = 2' >lib/b.ml
  $ echo 'let n = chosen_value' >lib/consumer.ml

  $ choose() {
  >   make_lockpkg settings <<EOF
  > (version 0.0.1)
  > (exported_env (= DEP_MODULE $1))
  > EOF
  > }
  $ obj=lib/.consumer_lib.objs/byte
  $ inputs() {
  >   dune rules --build-dir "$1" --format=json "$1/default/$obj/consumer.cmo" |
  >     jq_dune -c --arg dir "$obj" '.[] |
  >       {a: ruleHasDepFile($dir + "/a.cmi"),
  >        b: ruleHasDepFile($dir + "/b.cmi")}'
  > }

  $ choose A
  $ inputs _build
  {"a":true,"b":false}

After switching the export to B, the incremental scan still reports A.

  $ choose B
  $ inputs _build
  {"a":true,"b":false}
  $ test ! -e "_build/default/$obj/consumer.cmo"

A fresh scan of the same sources reports B.

  $ inputs _fresh
  {"a":false,"b":true}
  $ test ! -e "_fresh/default/$obj/consumer.cmo"
