Showcase that using same library name in two workspaces inside the same folder
is not possible at the moment

  $ cat > dune-project << EOF
  > (lang dune 3.13)
  > (package (name bar) (allow_empty))
  > (package (name baz) (allow_empty))
  > EOF

  $ cat > dune-workspace << EOF
  > (lang dune 3.13)
  > 
  > (context default)
  > 
  > (context
  >  (default
  >   (name alt-context)))
  > EOF
  $ cat > dune << EOF
  > (library
  >  (name foo)
  >  (enabled_if (= %{context_name} "default")))
  > (library
  >  (name foo)
  >  (enabled_if (= %{context_name} "alt-context")))
  > EOF
  $ cat > foo.ml <<EOF
  > let x = "foo"
  > EOF

  $ dune build --display=short
        ocamlc .foo.objs/byte/foo.{cmi,cmo,cmt} [alt-context]
        ocamlc .foo.objs/byte/foo.{cmi,cmo,cmt}
      ocamlopt .foo.objs/native/foo.{cmx,o} [alt-context]
        ocamlc foo.cma [alt-context]
      ocamlopt .foo.objs/native/foo.{cmx,o}
        ocamlc foo.cma
      ocamlopt foo.{a,cmxa} [alt-context]
      ocamlopt foo.{a,cmxa}
      ocamlopt foo.cmxs [alt-context]
      ocamlopt foo.cmxs

For public libraries

  $ cat > dune << EOF
  > (library
  >  (name foo)
  >  (public_name bar.foo)
  >  (enabled_if (= %{context_name} "default")))
  > (library
  >  (name foo)
  >  (public_name baz.foo)
  >  (enabled_if (= %{context_name} "alt-context")))
  > EOF

  $ dune build
  Error: Library foo is defined twice:
  - dune:7
  - dune:3
  [1]


In the same context

  $ cat > dune << EOF
  > (library
  >  (name foo))
  > (library
  >  (name foo))
  > EOF

If no public lib is available, the build finishes fine as there are no consumers of the libraries

  $ dune build

Let's add an exe to consume the library to trigger the error

  $ cat > dune << EOF
  > (library
  >  (name foo))
  > (library
  >  (name foo))
  > (executable
  >  (name main)
  >  (libraries foo))
  > EOF

  $ cat > main.ml <<EOF
  > let () = Foo.x
  > EOF

  $ dune build
  File "dune", line 3, characters 0-21:
  3 | (library
  4 |  (name foo))
  Error: Library "foo" appears for the second time in this directory
  [1]
