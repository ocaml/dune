For dune < 2.7 context_name is not allowed
  $ cat > dune-project <<EOF
  > (lang dune 2.6)
  > EOF
  $ dune exec ./foo.exe
  File "dune", line 3, characters 16-31:
  3 |  (enabled_if (= %{context_name} "default")))
                      ^^^^^^^^^^^^^^^
  Error: %{context_name} is only available since version 2.7 of the dune
  language. Please update your dune-project file to have (lang dune 2.7).
  [1]

For dune >= 2.7 context_name allowed
  $ cat > dune-project <<EOF
  > (lang dune 2.7)
  > EOF
  $ dune exec ./foo.exe
  bar
