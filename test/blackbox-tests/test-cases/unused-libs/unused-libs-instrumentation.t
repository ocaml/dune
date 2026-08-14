Test that the unused-libs alias observes libraries added by active
instrumentation backends.

  $ make_dune_project 3.25

  $ mkdir ppx
  $ cat > dune <<'EOF'
  > (library
  >  (name helper)
  >  (modules helper))
  > 
  > (executable
  >  (name main)
  >  (modules main)
  >  (instrumentation
  >   (backend noop)
  >   (libraries helper)))
  > EOF
  $ cat > main.ml <<'EOF'
  > let () = ()
  > EOF
  $ cat > helper.ml <<'EOF'
  > let message = "unused instrumentation library"
  > EOF

  $ cat > ppx/dune <<'EOF'
  > (library
  >  (name noop_ppx)
  >  (kind ppx_rewriter)
  >  (modules noop_ppx)
  >  (libraries ppxlib))
  > 
  > (library
  >  (name noop)
  >  (modules noop)
  >  (instrumentation.backend
  >   (ppx noop_ppx)))
  > EOF
  $ cat > ppx/noop_ppx.ml <<'EOF'
  > let () = Ppxlib.Driver.register_transformation "noop"
  > EOF
  $ cat > ppx/noop.ml <<'EOF'
  > EOF

Without instrumentation, the library is ignored.

  $ dune build @unused-libs

With instrumentation enabled, the library is part of the user-written
dependencies and is reported if unused.

  $ dune build --instrument-with noop @unused-libs
  File "dune", line 10, characters 13-19:
  10 |   (libraries helper)))
                    ^^^^^^
  Error: Unused libraries:
  - helper
  [1]

A library can also be archived with -linkall. In that case, it can be linked
for its side effects even without a module reference, but unused-libs still
reports it as unused because the check is import-based.

  $ cat > dune <<'EOF'
  > (library
  >  (name helper)
  >  (modules helper)
  >  (library_flags (-linkall)))
  > 
  > (executable
  >  (name main)
  >  (modes byte)
  >  (modules main)
  >  (instrumentation
  >   (backend noop)
  >   (libraries helper)))
  > EOF
  $ cat > helper.ml <<'EOF'
  > let () = print_endline "linkall instrumentation library"
  > EOF

  $ dune exec --instrument-with noop ./main.bc
  linkall instrumentation library

  $ dune build --instrument-with noop @unused-libs
  File "dune", line 12, characters 13-19:
  12 |   (libraries helper)))
                    ^^^^^^
  Error: Unused libraries:
  - helper
  [1]

Unused-libs is based on module imports, so it still reports the library as
unused even if the executable is linked with -linkall.

  $ cat > dune <<'EOF'
  > (library
  >  (name helper)
  >  (modules helper))
  > 
  > (executable
  >  (name main)
  >  (modules main)
  >  (link_flags -linkall)
  >  (instrumentation
  >   (backend noop)
  >   (libraries helper)))
  > EOF

  $ dune build --instrument-with noop @unused-libs
  File "dune", line 11, characters 13-19:
  11 |   (libraries helper)))
                    ^^^^^^
  Error: Unused libraries:
  - helper
  [1]
