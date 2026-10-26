Bytecode stub selection must include transitive installed libraries, including
libraries described only by META. See #9979.

  $ mkdir -p producer local
  $ cat >producer/dune-project <<'EOF'
  > (lang dune 3.13)
  > (package (name pkg))
  > EOF
  $ cat >producer/dune <<'EOF'
  > (library
  >  (name b)
  >  (public_name pkg.b)
  >  (modules b)
  >  (foreign_stubs (language c) (names b_stubs)))
  > (library
  >  (name facade)
  >  (public_name pkg.facade)
  >  (modules facade)
  >  (libraries pkg.b))
  > EOF
  $ echo 'external value : unit -> int = "b_value"' >producer/b.ml
  $ echo 'let value () = B.value ()' >producer/facade.ml
  $ cat >producer/b_stubs.c <<'EOF'
  > #include <caml/mlvalues.h>
  > CAMLprim value b_value(value unit) { return Val_int(20); }
  > EOF
  $ dune build --root producer @install
  $ dune install --root producer --prefix "$PWD/prefix" 2>/dev/null
  $ export OCAMLPATH="$PWD/prefix/lib"
  $ export CAML_LD_LIBRARY_PATH="$PWD/prefix/lib/stublibs"

The consumer mixes a local stub library with the installed facade. It does not
declare a direct dependency on b.

  $ make_dune_project_with_package 3.13 consumer
  $ cat >dune <<'EOF'
  > (data_only_dirs producer prefix)
  > (executable
  >  (name main)
  >  (modes byte)
  >  (libraries consumer.a pkg.facade))
  > EOF
  $ cat >local/dune <<'EOF'
  > (library
  >  (name a)
  >  (public_name consumer.a)
  >  (foreign_stubs (language c) (names a_stubs)))
  > EOF
  $ echo 'external value : unit -> int = "a_value"' >local/a.ml
  $ cat >local/a_stubs.c <<'EOF'
  > #include <caml/mlvalues.h>
  > CAMLprim value a_value(value unit) { return Val_int(10); }
  > EOF
  $ cat >main.ml <<'EOF'
  > let () = Printf.printf "%d %d\n" (A.value ()) (Facade.value ())
  > EOF

Supply the local library's runtime directory to ocamlrun explicitly. The
installed b is found through the facade and the installed library search path.

  $ dune build main.bc
  $ ocamlrun -I _build/default/local _build/default/main.bc
  10 20

Force findlib's META reader instead of dune-package. The transitive C primitive
must still be found at link time and called successfully at runtime.

  $ rm prefix/lib/pkg/dune-package
  $ test -f prefix/lib/pkg/META
  $ dune build main.bc
  $ ocamlrun -I _build/default/local _build/default/main.bc
  10 20
