  $ cat > dune-project <<EOF
  > (lang dune 3.2)
  > (package
  >   (name repro))
  > (package
  >    (name repro-other))
  > EOF

  $ cat > dune <<EOF
  > (library
  >  (name repro)
  >  (public_name repro)
  >  (modules foo))
  > (library
  >  (name repro_other)
  >  (public_name repro-other)
  >  (modules bar))
  > EOF

  $ touch foo.ml bar.ml

  $ dune build -p repro @install
  $ dune install -p repro --prefix prefix --dry-run
  Error: `-p PKGS' has no effect in `dune install'
  Hint: try running: dune install repro
  [1]
