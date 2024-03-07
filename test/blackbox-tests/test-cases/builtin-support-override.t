Create a library called `findlib.dynload`

  $ mkdir findlib
  $ cat > findlib/dune-project <<EOF
  > (lang dune 3.7)
  > (package (name findlib))
  > EOF
  $ cat > findlib/dune <<EOF
  > (library
  >  (name findlib_dynload)
  >  (public_name findlib.dynload)
  >  (wrapped false)
  >  (libraries findlib dynlink)
  >  (modules fl_dynload)
  >  (special_builtin_support findlib_dynload))
  > (library
  >  (public_name findlib)
  >  (modules findlib))
  > EOF
  $ cat >findlib/findlib.ml <<EOF
  > type x = Record_core
  > let record_package _ _ = assert false
  > let record_package_predicates _ _ = assert false
  > EOF
  $ touch findlib/fl_dynload.ml

  $ cat > dune-project <<EOF
  > (lang dune 3.7)
  > EOF

  $ mkdir lib
  $ cat > lib/dune <<EOF
  > (library
  >  (name foo_dynload)
  >  (libraries findlib.dynload))
  > EOF
  $ touch lib/foo_dynload.ml

  $ mkdir exe
  $ cat > exe/dune <<EOF
  > (executable
  >  (name the_exe)
  >  (libraries foo_dynload))
  > EOF
  $ touch exe/the_exe.ml

  $ dune build
