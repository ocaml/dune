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

Adding an `ocamllex` stanza for `a` shows a false error for `-p foo` (error is
in `bar`)

  $ cat > dune <<EOF
  > (ocamllex (modules a))
  > 
  > (library
  >  (name bar)
  >  (public_name bar) (modules a))
  > EOF

  $ dune build -p foo
  File "dune", line 1, characters 19-20:
  1 | (ocamllex (modules a))
                         ^
  Error: Module A doesn't exist.
  [1]

