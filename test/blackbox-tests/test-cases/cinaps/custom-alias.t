Custom alias for the cinaps

  $ cat > dune-project <<EOF
  > (lang dune 3.7)
  > (using cinaps 1.2)
  > EOF

  $ cat > dune <<EOF
  > (cinaps
  >  (files foo.ml)
  >  (alias foo))
  > EOF

  $ touch foo.ml

  $ dune build @foo --display short
        cinaps .cinaps.a7811055/cinaps.ml-gen
        ocamlc .cinaps.a7811055/.cinaps.eobjs/byte/dune__exe__Cinaps.{cmi,cmo,cmt}
      ocamlopt .cinaps.a7811055/.cinaps.eobjs/native/dune__exe__Cinaps.{cmx,o}
      ocamlopt .cinaps.a7811055/cinaps.exe
        cinaps alias foo
