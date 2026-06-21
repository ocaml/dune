When %{bin:foo} occurs in a bash action, it must be runnable. This requires
beginning the path with `./` when it occurs in the same directory, avoiding its
lookup in PATH.

  $ make_dune_project 3.24
  $ cat >> dune-project <<EOF
  > (package (name foopkg))
  > EOF

  $ cat > dune <<EOF
  > (executable (public_name foo) (package foopkg))
  > (rule
  >  (alias runtest)
  >  (action (bash %{bin:foo})))
  > EOF

  $ cat >foo.ml <<'EOF'
  > let () = print_endline "hello from foo"
  > EOF

  $ dune build @runtest
  hello from foo
