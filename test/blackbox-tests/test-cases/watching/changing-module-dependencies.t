Regression test for #5064: changing module dependencies in watch mode.

  $ echo '(lang dune 3.0)' > dune-project
  $ mkdir lib
  $ cat > lib/dune <<EOF
  > (library (name lib))
  > EOF
  $ cat > dune <<EOF
  > (executable (name x) (libraries lib))
  > EOF
  $ echo 'let hello = "Hello"' > lib/a.ml
  $ echo 'let world = "World"' > lib/b.ml
  $ echo 'let message = A.hello ^ ", " ^ B.world' > lib/lib.ml

  $ echo 'print_endline Lib.message' > x.ml

  $ start_dune

  $ build x.exe
  Success
  $ _build/default/x.exe
  Hello, World

Now let's make [lib/a.ml] depend on [lib/b.ml]. It doesn't work!

  $ cat > lib/a.ml <<EOF
  > let _ = B.world
  > let hello = "Hello"
  > EOF

  $ build x.exe
  Success

Let's try a manual restart.

  $ stop_dune_quiet

  $ start_dune

It works now!

  $ build x.exe
  Success

We're done.

  $ stop_dune_quiet

Merlin configurations for two consumers include the dependency's source
directories. Adding and removing a source directory must update both in the same
watch session.

  $ echo '(include_subdirs unqualified)' >> lib/dune
  $ mkdir consumer
  $ cat > consumer/dune <<EOF
  > (library (name consumer) (libraries lib))
  > EOF
  $ touch consumer/consumer.ml
  $ show_dependency_dirs () {
  >   for dir in . consumer; do
  >     printf '%s: ' "$dir"
  >     dune ocaml merlin dump-config --format=json "$PWD/$dir" \
  >       | jq -c --arg root "$PWD/" '
  >         [.[0].config[] | select(.[0] == "S") | .[1] | ltrimstr($root)
  >          | select(. == "lib" or startswith("lib/"))]'
  >   done
  > }

  $ start_dune
  $ build .merlin-conf/exe-x consumer/.merlin-conf/lib-consumer
  Success
  $ show_dependency_dirs
  .: ["lib"]
  consumer: ["lib"]

Flush pending file events before requesting each rebuild.

  $ mkdir lib/sub
  $ touch lib/sub/c.ml
  $ with_timeout dune rpc flush-file-watcher --wait
  $ build .merlin-conf/exe-x consumer/.merlin-conf/lib-consumer
  Success
  $ show_dependency_dirs
  .: ["lib","lib/sub"]
  consumer: ["lib","lib/sub"]

  $ rm lib/sub/c.ml
  $ rmdir lib/sub
  $ with_timeout dune rpc flush-file-watcher --wait
  $ build .merlin-conf/exe-x consumer/.merlin-conf/lib-consumer
  Success
  $ show_dependency_dirs
  .: ["lib"]
  consumer: ["lib"]
  $ stop_dune_quiet

Enabling a staged PPX can change a module's dependencies without changing its
source. A watch server must discard the old ocamldep command in that case.

  $ make_dune_project 3.26
  $ mkdir ppx
  $ cat > ppx/dune <<'EOF'
  > (library
  >  (name select_ppx)
  >  (kind ppx_rewriter)
  >  (libraries compiler-libs.common)
  >  (ppx.driver (main Select_ppx.main)))
  > EOF
  $ cat > ppx/select_ppx.ml <<'EOF'
  > let main () =
  >   let expr self (expr : Parsetree.expression) =
  >     match expr.pexp_desc with
  >     | Pexp_ident { txt = Lident "chosen_value"; _ } ->
  >       Parse.expression (Lexing.from_string "Second.value")
  >     | _ -> Ast_mapper.default_mapper.expr self expr
  >   in
  >   Ast_mapper.run_main (fun _ -> { Ast_mapper.default_mapper with expr })
  > EOF
  $ echo 'let value = "first"' > first.ml
  $ echo 'let value = "second"' > second.ml
  $ echo 'let chosen_value = First.value
  > let () = print_endline chosen_value' > x.ml
  $ configure() {
  >   cat > dune <<EOF
  > (executable
  >  (name x)
  >  (modules x first second)
  >  $1)
  > EOF
  > }
  $ configure ''
  $ start_dune --sandbox hardlink --cache disabled
  $ build x.exe
  Success
  $ _build/default/x.exe
  first
  $ configure '(preprocess (staged_pps select_ppx))'
  $ with_timeout dune rpc flush-file-watcher --wait
  $ build x.exe
  Failure
  [1]
  $ stop_dune_quiet
  $ grep -o 'Error: The module Second is an alias for module .*which is missing' .#dune-output
  Error: The module Second is an alias for module Dune__exe__Second, which is missing
  $ test ! -e _build/default/.x.eobjs/byte/dune__exe__Second.cmi

A fresh build from those same files discovers `Second` and prints `second`.

  $ dune exec --build-dir _fresh --sandbox hardlink --cache disabled ./x.exe
  second
