The bytecode linker searches its working directory before any -I directory.
An unrelated root-level DLL can therefore shadow an installed dependency even
when no local stub directory is passed on the command line. See #9979.

  $ mkdir -p producer app relative-stubs
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
  $ echo 'external value : unit -> int = "selected_b"' >producer/b.ml
  $ cat >producer/b_stubs.c <<'EOF'
  > #include <caml/mlvalues.h>
  > CAMLprim value selected_b(value unit) { return Val_int(2); }
  > EOF
  $ dune build --root producer @install
  $ dune install --root producer --prefix "$PWD/prefix" 2>/dev/null
  $ export OCAMLPATH="$PWD/prefix/lib"
  $ ext_dll=$(ocamlc -config-var ext_dll)

The root library is built separately and is not a dependency of main.

  $ make_dune_project 3.13
  $ cat >dune <<'EOF'
  > (data_only_dirs producer prefix)
  > (library
  >  (name b)
  >  (modules b)
  >  (foreign_stubs (language c) (names unrelated)))
  > EOF
  $ touch b.ml
  $ cat >unrelated.c <<'EOF'
  > #include <caml/mlvalues.h>
  > CAMLprim value unrelated_b(value unit) { return Val_int(0); }
  > EOF
  $ dune build dllb_stubs$ext_dll

The executable also uses an explicit relative -I flag. Changing the linker's
working directory must preserve the meaning of this flag.

  $ cat >relative-stubs/dune <<'EOF'
  > (foreign_library
  >  (archive_name relative)
  >  (language c)
  >  (names relative))
  > EOF
  $ cat >relative-stubs/relative.c <<'EOF'
  > #include <caml/mlvalues.h>
  > CAMLprim value relative_value(value unit) { return Val_int(3); }
  > EOF
  $ cat >app/dune <<'EOF'
  > (executable
  >  (name main)
  >  (modes byte)
  >  (libraries pkg.b)
  >  (link_deps ../relative-stubs/dllrelative%{ext_dll})
  >  (link_flags (:standard -I relative-stubs -dllib -lrelative)))
  > EOF
  $ cat >app/main.ml <<'EOF'
  > external relative : unit -> int = "relative_value"
  > let () = Printf.printf "%d %d\n" (B.value ()) (relative ())
  > EOF
  $ dune build app/main.bc --sandbox=none
  File "_none_", line 1:
  Error: Error while linking app/.main.eobjs/byte/dune__exe__Main.cmo:
         The external function selected_b is not available
  [1]

Removing the unrelated DLL lets the same executable and relative flags
link successfully.

  $ rm _build/default/dllb_stubs$ext_dll
  $ dune build app/main.bc --sandbox=none
