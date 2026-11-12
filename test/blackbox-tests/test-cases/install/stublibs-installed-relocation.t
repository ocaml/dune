A build-time stub selection directory must not become a runtime dependency of
installed bytecode. The installed program must run after moving the prefix and
making the original build trees unavailable. See #9979.

  $ mkdir -p producer local elsewhere
  $ cat >producer/dune-project <<'EOF'
  > (lang dune 3.13)
  > (package (name pkg))
  > EOF
  $ cat >producer/dune <<'EOF'
  > (library
  >  (name b)
  >  (public_name pkg.b)
  >  (foreign_stubs (language c) (names b_stubs)))
  > EOF
  $ echo 'external value : unit -> int = "b_value"' >producer/b.ml
  $ cat >producer/b_stubs.c <<'EOF'
  > #include <caml/mlvalues.h>
  > CAMLprim value b_value(value unit) { return Val_int(20); }
  > EOF
  $ dune build --root producer @install
  $ dune install --root producer --prefix "$PWD/prefix" 2>/dev/null
  $ export OCAMLPATH="$PWD/prefix/lib"

The program mixes a local library with the already installed dependency.

  $ make_dune_project_with_package 3.13 app
  $ cat >dune <<'EOF'
  > (data_only_dirs producer prefix elsewhere)
  > (executable
  >  (name main)
  >  (modes byte)
  >  (libraries app.a pkg.b))
  > (install
  >  (section bin)
  >  (files main.bc))
  > EOF
  $ cat >local/dune <<'EOF'
  > (library
  >  (name a)
  >  (public_name app.a)
  >  (foreign_stubs (language c) (names a_stubs)))
  > EOF
  $ echo 'external value : unit -> int = "a_value"' >local/a.ml
  $ cat >local/a_stubs.c <<'EOF'
  > #include <caml/mlvalues.h>
  > CAMLprim value a_value(value unit) { return Val_int(10); }
  > EOF
  $ cat >main.ml <<'EOF'
  > let () = Printf.printf "%d %d\n" (A.value ()) (B.value ())
  > EOF
  $ dune build @install
  $ dune install --prefix "$PWD/prefix" 2>/dev/null
  $ CAML_LD_LIBRARY_PATH="$PWD/prefix/lib/stublibs" \
  >   ocamlrun prefix/bin/main.bc
  10 20

Run from another directory with only the relocated installed stubs available
at their expected paths. Neither original build directory nor prefix exists.

  $ mv _build saved-build
  $ mv producer/_build producer/saved-build
  $ mv prefix relocated
  $ unset OCAMLPATH
  $ export CAML_LD_LIBRARY_PATH="$PWD/relocated/lib/stublibs"
  $ (cd elsewhere && ocamlrun ../relocated/bin/main.bc)
  10 20
