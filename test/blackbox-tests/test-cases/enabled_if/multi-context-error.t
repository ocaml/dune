Demonstrate errors coming from a different context:

  $ cat > dune-project << EOF
  > (lang dune 3.15)
  > (package (name bar) (allow_empty))
  > (package (name baz) (allow_empty))
  > EOF

  $ cat > dune-workspace << EOF
  > (lang dune 3.15)
  > 
  > (context default)
  > 
  > (context
  >  (default
  >   (name other)))
  > EOF
  $ cat > dune << EOF
  > (rule
  >  (action (write-file foo bar))
  >  (enabled_if (= %{context_name} "default")))
  > (rule
  >  (targets foo)
  >  (action (system "exit 1"))
  >  (enabled_if (<> %{context_name} "default")))
  > EOF

  $ cat > foo.ml <<EOF
  > let t = Str.regexp
  > EOF

  $ dune build
  File "dune", lines 4-7, characters 0-94:
  4 | (rule
  5 |  (targets foo)
  6 |  (action (system "exit 1"))
  7 |  (enabled_if (<> %{context_name} "default")))
  Context: other
  Command exited with code 1.
  [1]
