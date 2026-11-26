A JS executable links the runtime files of its locked libraries. A natively
loaded locked library declares them with (javascript_files).

  $ make_lockdir
  $ make_named_package_project main 3.25 foo
  $ mkdir _foo

[foo] is a dune project whose library ships a JavaScript primitive.

  $ (cd _foo && make_dune_project_with_package 3.25 foo)
  $ cat > _foo/dune <<'EOF2'
  > (library (public_name foo) (js_of_ocaml (javascript_files foo_runtime.js)))
  > EOF2
  $ cat > _foo/foo.ml <<'EOF2'
  > external prim : unit -> int = "foo_prim"
  > let value = prim ()
  > EOF2
  $ cat > _foo/foo_runtime.js <<'EOF2'
  > //Provides: foo_prim
  > function foo_prim(unit) { return 42; }
  > EOF2
  $ make_dune_source_lockpkg native foo "$PWD/_foo"

A JS executable uses it.

  $ cat > dune <<'EOF2'
  > (executable (name mounted) (modes js) (libraries foo))
  > EOF2
  $ echo 'let () = print_endline (string_of_int Foo.value)' > mounted.ml
  $ dune build mounted.bc.js
  $ node _build/default/mounted.bc.js
  42

Editing only the mounted library's JavaScript source re-links its consumer.

  $ cat > _foo/foo_runtime.js <<'EOF2'
  > //Provides: foo_prim
  > function foo_prim(unit) { return 84; }
  > EOF2
  $ dune build mounted.bc.js
  $ node _build/default/mounted.bc.js
  84
