Show an error where `ocamllex` doesn't respect `-p <pkg>`

  $ cat > dune-project <<EOF
  > (lang dune 3.22)
  > (package (name bar))
  > (package (name foo) (allow_empty))
  > EOF
  $ cat > dune <<EOF
  > (library
  >  (name bar)
  >  (public_name bar) (modules a))
  > EOF

  $ touch foo.opam bar.opam

Building works even if `a` is missing

  $ dune build -p foo

Adding an `ocamllex` stanza for `a` still let's the build through for `-p foo`

  $ cat > dune <<EOF
  > (ocamllex (modules a))
  > 
  > (library
  >  (name bar)
  >  (public_name bar) (modules a))
  > EOF

  $ dune build -p foo

Building `bar` still fails

  $ dune build -p bar
  File "dune", line 5, characters 28-29:
  5 |  (public_name bar) (modules a))
                                  ^
  Error: Module A doesn't exist.
  [1]
