A locked opaque recipe must receive workspace package variables and installed
artifacts through the same materialiser as other recipe dependencies.

  $ make_workspace_lib_package

The recipe checks metadata and section paths, compiles against the workspace
library, and records the result. Merely adding an OCAMLPATH entry is not enough:
the library files must be materialised and tracked as dependencies.

  $ make_lockdir
  $ make_lockpkg consumer <<EOF
  > (version 0.0.1)
  > (depends workspace-lib)
  > (build
  >  (system "set -eu; test '%{pkg:workspace-lib:name}' = workspace-lib; test '%{pkg:workspace-lib:version}' = dev; test '%{pkg:workspace-lib:installed}' = true; test -f '%{pkg:workspace-lib:lib}/workspace_lib.cma'; echo 'let () = print_endline Workspace_lib.greeting' > main.ml; ocamlc -I '%{pkg:workspace-lib:lib}' workspace_lib.cma main.ml -o main.exe; ./main.exe > result"))
  > (install
  >  (system "mkdir -p %{pkg-self:share}; cp result %{pkg-self:share}/result"))
  > EOF

A rule depends on the lock-dir package:

  $ write_lockdir_consumer_rule

  $ dune build out
  $ result="$(get_build_pkg_dir consumer)/_install/share/consumer/result"
  $ cat "$result"
  Hello from workspace-lib!

Workspace edits invalidate the locked consumer without relocking.

  $ echo 'let greeting = "updated library"' > src/workspace_lib.ml
  $ dune build out
  $ cat "$result"
  updated library

The workspace provider can itself depend on another locked package. The shared
layout must contain the transitive library needed to link the opaque consumer,
without making that library's variables a direct recipe dependency.

  $ make_named_package_project workspace-lib 3.25 leaf
  $ echo '(library (name workspace_lib) (public_name workspace-lib) (modes byte) (libraries leaf))' > src/dune
  $ echo 'let greeting = Leaf.greeting' > src/workspace_lib.ml
  $ mkdir _leaf
  $ (cd _leaf && make_dune_project_with_package 3.25 leaf)
  $ echo '(library (public_name leaf) (modes byte))' > _leaf/dune
  $ echo 'let greeting = "dune leaf"' > _leaf/leaf.ml
  $ make_dune_source_lockpkg native leaf "$PWD/_leaf"
  $ make_lockpkg consumer <<'EOF'
  > (version 0.0.1)
  > (depends workspace-lib)
  > (build
  >  (system "set -eu; echo 'let () = print_endline Workspace_lib.greeting' > main.ml; ocamlc -I '%{pkg:workspace-lib:lib}/../leaf' -I '%{pkg:workspace-lib:lib}' leaf.cma workspace_lib.cma main.ml -o main.exe; ./main.exe > result"))
  > (install (system "mkdir -p %{pkg-self:share}; cp result %{pkg-self:share}/result"))
  > EOF
  $ dune build out
  $ cat "$result"
  dune leaf
  $ test ! -e $pkg_root/leaf/_install/opaque-recipe

The same leaf source built by an opaque recipe crosses the same way:

  $ make_dune_source_lockpkg opaque leaf "$PWD/_leaf"
  $ dune build out
  $ cat "$result"
  dune leaf
  $ cat $pkg_root/leaf/_install/opaque-recipe
  opaque

So does a hand-written opaque recipe without a Dune project:

  $ make_lockpkg leaf <<'EOF'
  > (version 1.0)
  > (build
  >  (system "set -eu; echo 'let greeting = \"opaque leaf\"' > leaf.ml; ocamlc -c leaf.ml; ocamlc -a leaf.cmo -o leaf.cma; echo 'archive(byte) = \"leaf.cma\"' > META; echo 'lib: [ \"leaf.cma\" \"leaf.cmi\" \"META\" ]' > leaf.install"))
  > EOF
  $ dune build out
  $ cat "$result"
  opaque leaf
