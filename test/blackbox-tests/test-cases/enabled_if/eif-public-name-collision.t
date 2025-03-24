Show that public library names can't be defined twice, even in different
contexts

  $ mkdir -p a b

  $ cat > dune-workspace << EOF
  > (lang dune 3.13)
  > (context default)
  > (context
  >  (default
  >   (name melange)))
  > EOF
  $ cat > dune-project << EOF
  > (lang dune 3.13)
  > (package (name foo) (allow_empty))
  > EOF

  $ cat > a/dune << EOF
  > (library
  >  (name foo)
  >  (public_name foo.lib)
  >  (enabled_if (= %{context_name} "default")))
  > EOF
  $ cat > a/foo.ml << EOF
  > let x = "hello"
  > EOF

  $ cat > b/dune << EOF
  > (library
  >  (name foo)
  >  (public_name foo.lib)
  >  (enabled_if (= %{context_name} "melange")))
  > EOF

Without any consumers of the libraries

  $ dune build

With some consumer

  $ cat > dune << EOF
  > (executable
  >  (name main)
  >  (libraries foo.lib)
  >  (enabled_if (= %{context_name} "default")))
  > EOF

  $ cat > main.ml <<EOF
  > let () = print_endline Foo.x
  > EOF

  $ dune exec ./main.exe
  hello

