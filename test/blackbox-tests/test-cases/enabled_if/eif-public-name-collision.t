Public libraries using the same `public_name`, in different contexts

  $ mkdir -p a b

  $ cat > dune-workspace << EOF
  > (lang dune 3.13)
  > 
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
  >  (name libname)
  >  (public_name foo.lib)
  >  (enabled_if (= %{context_name} "default")))
  > EOF

  $ cat > b/dune << EOF
  > (library
  >  (name libname)
  >  (public_name foo.lib)
  >  (enabled_if (= %{context_name} "melange")))
  > EOF

Without any consumers of the libraries

  $ dune build
  File "b/dune", line 3, characters 14-21:
  3 |  (public_name foo.lib)
                    ^^^^^^^
  Error: Public library foo.lib is defined twice:
  - a/dune:3
  - b/dune:3
  [1]

With some consumer

  $ cat > dune << EOF
  > (executable
  >  (name main)
  >  (libraries foo.lib)
  >  (enabled_if (= %{context_name} "default")))
  > EOF

  $ cat > main.ml <<EOF
  > let () = Foo.x
  > EOF

  $ dune build
  File "b/dune", line 3, characters 14-21:
  3 |  (public_name foo.lib)
                    ^^^^^^^
  Error: Public library foo.lib is defined twice:
  - a/dune:3
  - b/dune:3
  [1]

