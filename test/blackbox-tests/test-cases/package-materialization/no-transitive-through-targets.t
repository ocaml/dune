Package dependencies do not propagate transitively through build
targets. If rule A depends on (package pkga) and rule B depends on A
and (package pkgb), then B's layout only contains pkgb, not pkga.
Declare what you need explicitly.

  $ cat >dune-project <<EOF
  > (lang dune 3.24)
  > (package (name pkga))
  > (package (name pkgb))
  > EOF

  $ mkdir a-src b-src

  $ cat >a-src/dune <<EOF
  > (library (public_name pkga))
  > EOF

  $ cat >a-src/alib.ml <<EOF
  > let x = 1
  > EOF

  $ cat >b-src/dune <<EOF
  > (library (public_name pkgb))
  > EOF

  $ cat >b-src/blib.ml <<EOF
  > let y = 2
  > EOF

Rule A declares (package pkga). Rule B depends on target-a and
declares (package pkgb), but B's action tries to find pkga via
ocamlfind. ocamlfind fails because pkga is not in B's package set:

  $ cat >dune <<'EOF'
  > (rule
  >  (deps (package pkga))
  >  (action (with-stdout-to target-a (echo "built with pkga"))))
  > (rule
  >  (deps target-a (package pkgb))
  >  (action (with-stdout-to target-b
  >   (run ocamlfind query pkga))))
  > EOF

  $ dune build target-b 2>&1
  File "dune", lines 4-7, characters 0-103:
  4 | (rule
  5 |  (deps target-a (package pkgb))
  6 |  (action (with-stdout-to target-b
  7 |   (run ocamlfind query pkga))))
  ocamlfind: Package `pkga' not found
  [1]

pkgb IS findable because B declared it. The build succeeds.

  $ cat >dune <<'EOF'
  > (rule
  >  (deps (package pkgb))
  >  (action (with-stdout-to target-ok
  >   (run ocamlfind query pkgb))))
  > EOF

  $ dune build target-ok
