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
  $ result="$(get_build_pkg_dir consumer)/target/share/consumer/result"
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

  $ cat > dune-project <<'EOF'
  > (lang dune 3.25)
  > (package (name workspace-lib) (depends leaf))
  > EOF
  $ echo '(library (name workspace_lib) (public_name workspace-lib) (modes byte) (libraries leaf))' > src/dune
  $ echo 'let greeting = Leaf.greeting' > src/workspace_lib.ml
  $ mkdir _leaf
  $ cat > _leaf/dune-project <<'EOF'
  > (lang dune 3.25)
  > (package (name leaf))
  > EOF
  $ echo '(library (public_name leaf) (modes byte))' > _leaf/dune
  $ echo 'let greeting = "native leaf"' > _leaf/leaf.ml
  $ make_lockpkg leaf <<EOF
  > (version 1.0)
  > (source (copy $PWD/_leaf))
  > (build (run false))
  > EOF
  $ make_lockpkg consumer <<'EOF'
  > (version 0.0.1)
  > (depends workspace-lib)
  > (build
  >  (system "set -eu; echo 'let () = print_endline Workspace_lib.greeting' > main.ml; ocamlc -I '%{pkg:workspace-lib:lib}/../leaf' -I '%{pkg:workspace-lib:lib}' leaf.cma workspace_lib.cma main.ml -o main.exe; ./main.exe > result"))
  > (install (system "mkdir -p %{pkg-self:share}; cp result %{pkg-self:share}/result"))
  > EOF
  $ dune build out
  $ cat "$result"
  native leaf

Builder choice does not change the crossing: make the leaf an opaque package.

  $ make_lockpkg leaf <<'EOF'
  > (version 1.0)
  > (build
  >  (system "set -eu; echo 'let greeting = \"opaque leaf\"' > leaf.ml; ocamlc -c leaf.ml; ocamlc -a leaf.cmo -o leaf.cma; echo 'archive(byte) = \"leaf.cma\"' > META; echo 'lib: [ \"leaf.cma\" \"leaf.cmi\" \"META\" ]' > leaf.install"))
  > EOF
  $ dune build out
  $ cat "$result"
  opaque leaf
