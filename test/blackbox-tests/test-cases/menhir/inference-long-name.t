Menhir inference supports long parser names in qualified subdirectories.
With a 228-character basename, the path-based helper name from #16464 would
exceed the 255-byte filename limit.

  $ make_menhir_project 3.25 3.0
  $ cat >dune <<'EOF'
  > (include_subdirs qualified)
  > (library
  >  (name foo))
  > EOF
  $ mkdir lang
  $ touch lang/lang.ml
  $ parser="p$(printf '%0227d' 0)"
  $ cat >lang/dune <<EOF
  > (menhir
  >  (modules $parser))
  > EOF
  $ cat >"lang/$parser.mly" <<'EOF'
  > %token EOF
  > %start <unit> main
  > %%
  > main: EOF { () }
  > EOF
  $ dune build foo.cma
