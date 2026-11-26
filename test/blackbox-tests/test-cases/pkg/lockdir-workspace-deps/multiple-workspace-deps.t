A lock-dir package depends on multiple workspace packages.

  $ make_dune_project 3.24
  $ cat >> dune-project <<EOF
  > (package (name ws-lib-a))
  > (package (name ws-lib-b))
  > EOF

  $ mkdir src-a src-b
  $ cat > src-a/dune <<EOF
  > (library
  >  (name ws_lib_a)
  >  (public_name ws-lib-a))
  > EOF
  $ cat > src-a/ws_lib_a.ml <<EOF
  > let value_a = "from lib a"
  > EOF

  $ cat > src-b/dune <<EOF
  > (library
  >  (name ws_lib_b)
  >  (public_name ws-lib-b))
  > EOF
  $ cat > src-b/ws_lib_b.ml <<EOF
  > let value_b = "from lib b"
  > EOF

The lock dir contains one package "consumer" that declares both
workspace libraries as dependencies:

  $ make_lockdir
  $ make_lockpkg consumer <<'EOF'
  > (version 0.0.1)
  > (depends ws-lib-a ws-lib-b)
  > (build
  >  (progn
  >   (system "echo 'let () =
  >     print_string Ws_lib_a.value_a;
  >     print_string Ws_lib_b.value_b' > main.ml")
  >   (run ocamlc
  >    -I %{pkg:ws-lib-a:lib}
  >    -I %{pkg:ws-lib-b:lib}
  >    ws_lib_a.cma ws_lib_b.cma main.ml -o main.exe)
  >   (system "test \"$(./main.exe)\" = 'from lib afrom lib b'")
  >   (run echo "building consumer")))
  > EOF

A rule depends on the lock-dir package:

  $ write_lockdir_consumer_rule

The recipe links and runs against both workspace libraries from its dependency
layout, checking that each provider's value is available.

  $ dune build out
  building consumer
