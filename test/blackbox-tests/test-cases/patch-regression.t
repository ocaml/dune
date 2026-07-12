  $ echo "(lang dune 3.16)" > dune-project
  $ cat > dune << EOF
  > (executable
  >  (name repro))
  > (data_only_dirs tree)
  > (rule
  >  (targets repro.ml)
  >  (deps
  >   (source_tree tree)
  >   patch.patch)
  >  (action
  >   (no-infer
  >    (progn
  >     (run chmod +w tree/template.ml)
  >     (with-stdin-from
  >      ./patch.patch
  >      (run patch -p1))
  >     (with-stdout-to
  >      repro.ml
  >      (cat tree/template.ml))))))
  > EOF

Patch some file in tree/
  $ cat > patch.patch << EOF
  > diff -u a/tree/template.ml b/tree/template.ml
  > --- a/tree/template.ml
  > +++ b/tree/template.ml
  > @@ -1,1 +1,1 @@
  > -let () = print_endline "Hello, Planet!"
  > +let () = print_endline "Hello, World!"
  > EOF

  $ mkdir tree
  $ echo "let () = print_endline \"Hello, Planet!\"" > tree/template.ml
  $ dune exec -- ./repro.exe

Change the patch. Prior to 3.23.0 this worked
  $ cat > patch.patch << EOF
  > diff -u a/tree/template.ml b/tree/template.ml
  > --- a/tree/template.ml
  > +++ b/tree/template.ml
  > @@ -1,1 +1,1 @@
  > -let () = print_endline "Hello, Planet!"
  > +let () = print_endline "Hello, World !"
  > EOF

  $ dune exec -- ./repro.exe
