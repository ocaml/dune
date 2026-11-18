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

Switch the export to B without changing the consumer or the PPX.

  $ choose B
  $ inputs _build
  {"a":true,"b":false}
  $ test ! -e "_build/default/$obj/consumer.cmo"

A fresh scan of the same sources reports B.

  $ inputs _fresh
  {"a":false,"b":true}
  $ test ! -e "_fresh/default/$obj/consumer.cmo"

The directory can also configure the rewriter without a lock context.
Changing a binding in the parent directory should update the scan, while
unrelated variables inherited from the shell are not implicit inputs.

  $ cat >dune-workspace <<'EOF'
  > (lang dune 3.25)
  > (pkg disabled)
  > EOF
  $ export AMBIENT_ONLY=present
  $ mkdir app
  $ echo '(executable (name main) (libraries consumer_lib))' >app/dune
  $ echo 'let () = print_int Consumer.n; print_newline ()' >app/main.ml
  $ configure() { printf '(env (_ (env_vars (DEP_MODULE %s))))\n' "$1" >dune; }
  $ configure A
  $ inputs _build
  {"a":true,"b":false}
  $ dune exec ./app/main.exe
  1
  $ configure B
  $ inputs _build
  {"a":true,"b":false}
  $ dune exec ./app/main.exe
  1
  $ dune rules --format=json "_build/default/$obj/consumer.cmo" |
  >   jq -c '.[] | .deps | {configured: (index({Env:"DEP_MODULE"}) != null),
  >                         ambient: (index({Env:"AMBIENT_ONLY"}) != null)}'
  {"configured":false,"ambient":false}
