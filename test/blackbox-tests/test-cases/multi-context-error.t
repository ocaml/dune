  $ cat > dune-project << EOF
  > (lang dune 3.15)
  > (using melange 0.1)
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
  >   (name melange)))
  > EOF
  $ cat > dune << EOF
  > (library
  >  (name foo)
  >  (public_name bar.foo)
  >  (libraries str)
  >  (enabled_if (= %{context_name} "default")))
  > (library
  >  (name foo)
  >  (public_name baz.foo)
  >  (modes melange)
  >  (enabled_if (= %{context_name} "melange")))
  > EOF

  $ cat > foo.ml <<EOF
  > let t = Str.regexp
  > EOF

  $ dune build
  Context: melange
  File "foo.ml", line 1, characters 8-18:
  1 | let t = Str.regexp
              ^^^^^^^^^^
  Error: Unbound module Str
  [1]
