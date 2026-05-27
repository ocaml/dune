A consumer references a module from a preprocessed library; that
preprocessed module's interface mentions a type from a leaf library
that the consumer never names syntactically. Pins that the consumer's
compile correctly tracks `leaf`'s `.cmi` as a sandbox-required dep.

  $ make_dune_project 3.24

`leaf` exposes `Leaf.t`:

  $ mkdir leaf
  $ cat > leaf/dune <<EOF
  > (library (name leaf))
  > EOF
  $ cat > leaf/leaf.ml <<EOF
  > type t = int
  > let zero : t = 0
  > EOF

`middle` depends on `leaf`; its single module is preprocessed via
`(preprocess (action ...))`, and its interface mentions `Leaf.t`:

  $ mkdir middle
  $ cat > middle/dune <<EOF
  > (library
  >  (name middle)
  >  (libraries leaf)
  >  (preprocess (action (run cat %{input-file}))))
  > EOF
  $ cat > middle/middle.mli <<EOF
  > val identity : Leaf.t -> Leaf.t
  > EOF
  $ cat > middle/middle.ml <<EOF
  > let identity x = x
  > EOF

`consumer` depends on `middle`; references `Middle.identity` but
never names `Leaf` in source. Applying `Middle.identity` to a
literal `0` forces the compiler to unify `int` with `Leaf.t`, which
requires loading `leaf.cmi`. `ocamldep` on this source reports
only `Middle`, since `Leaf` is not named.

  $ mkdir consumer
  $ cat > consumer/dune <<EOF
  > (executable (name consumer) (libraries middle))
  > EOF
  $ cat > consumer/consumer.ml <<EOF
  > let _ = Middle.identity 0
  > EOF

  $ dune build --sandbox=copy consumer/consumer.exe
