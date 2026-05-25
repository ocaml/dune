Melange copies Reason sources into the build directory with a line directive
before refmt converts them to OCaml. If the module then goes through a PPX, the
compiler diagnostic should still print the original Reason source line.

  $ mkdir -p cases/ppx cases/src

  $ cat > cases/dune-project <<'EOF'
  > (lang dune 3.18)
  > (using melange 0.1)
  > EOF

  $ cat > cases/ppx/dune <<'EOF'
  > (library
  >  (name ppx_noop)
  >  (kind ppx_rewriter)
  >  (modules ppx_noop)
  >  (libraries ppxlib))
  > EOF

  $ cat > cases/ppx/ppx_noop.ml <<'EOF'
  > let () = Ppxlib.Driver.register_transformation "ppx_noop"
  > EOF

  $ cat > cases/src/dune <<'EOF'
  > (library
  >  (name reason_impl)
  >  (modes melange)
  >  (modules x)
  >  (preprocess
  >   (pps ppx_noop)))
  > EOF

  $ cat > cases/src/x.re <<'EOF'
  > let ignored = 0
  > let x = 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + "reason"
  > EOF

  $ dune build --root cases --sandbox=symlink @src/all
  Entering directory 'cases'
  File "src/x.re", line 2, characters 40-48:
   |  1 + 1 + 1 + 1 + 1 + 1 + "reason"
                                      ^^^^^^^^
  Error: This constant has type string but an expression was expected of type
           int
  Leaving directory 'cases'
  [1]
