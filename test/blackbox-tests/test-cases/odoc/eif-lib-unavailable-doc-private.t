Show behavior of doc-private alias when it's part of an unavailable library

  $ cat > dune-project << EOF
  > (lang dune 3.13)
  > EOF

  $ cat > dune << EOF
  > (library
  >  (name foo)
  >  (enabled_if (= %{context_name} "unavailable")))
  > EOF
  $ cat > foo.ml <<EOF
  > let x = "foo"
  > EOF

  $ dune build @doc-private
  Error: No rule found for alias _doc/_html/foo@26bb1931b3ad/doc
  -> required by alias doc-private
  [1]
