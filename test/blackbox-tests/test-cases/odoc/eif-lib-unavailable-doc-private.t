Show behavior of doc-private alias when it's part of an unavailable library

  $ make_dune_project 3.13

  $ cat > dune << EOF
  > (library
  >  (name foo)
  >  (enabled_if (= %{context_name} "unavailable")))
  > EOF
  $ cat > foo.ml <<EOF
  > let x = "foo"
  > EOF

  $ dune build @doc-private
