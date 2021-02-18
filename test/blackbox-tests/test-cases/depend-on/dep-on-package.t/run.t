# Test dependency on a package in the tree

  $ mkdir a b

  $ cat >a/dune-project <<EOF
  > (lang dune 2.9)
  > (package (name a))
  > EOF

  $ cat >a/dune <<EOF
  > (library (name a) (public_name a))
  > EOF

  $ cat > a/a.ml <<EOF
  > let () = ()
  > EOF

  $ cat >b/dune-project <<EOF
  > (lang dune 2.9)
  > (package (name b))
  > EOF

  $ cat >b/dune <<'EOF'
  > (rule (alias runtest) (deps (package a)) (action (bash "cat \"$(ocamlfind query a)\"/a.ml")))
  > EOF

  $ dune build @b/runtest

  $ echo 'let () = Printf.printf "hello world"' >> a/a.ml

  $ dune build @b/runtest

