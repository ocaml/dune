Menhir inference must also avoid shadowed aliases before OCaml 4.08 (#8989).
Report an older compiler version to exercise this path on current compilers.

  $ real_ocamlc=$(command -v ocamlc)
  $ mkdir compiler
  $ cat >compiler/ocamlc <<EOF
  > #!/bin/sh
  > case "\$1" in
  >   -config) "$real_ocamlc" "\$@" | sed 's/^version: .*/version: 4.07.1/' ;;
  >   *) exec "$real_ocamlc" "\$@" ;;
  > esac
  > EOF
  $ chmod +x compiler/ocamlc
  $ ln -s "$(command -v ocamldep)" compiler/ocamldep
  $ export PATH="$PWD/compiler:$PATH"

Use nested groups to exercise multiple alias opens. The parser header must
still allow first-class module unpacking and generative functor applications.

  $ make_menhir_project 3.25 3.0
  $ cat >dune <<'EOF'
  > (include_subdirs qualified)
  > (library
  >  (name lib)
  >  (flags (:standard -w +63 -warn-error +63)))
  > EOF
  $ mkdir -p outer/inner
  $ echo 'module Inner = Inner' >outer/outer.ml
  $ echo 'module type S = sig end' >outer/ast.ml
  $ echo 'type t = T let x = T' >outer/inner/m.ml
  $ echo 'type t val x : t' >outer/inner/m.mli
  $ cat >outer/inner/dune <<'EOF'
  > (menhir
  >  (modules inner))
  > EOF
  $ cat >outer/inner/inner.mly <<'EOF'
  > %{
  > module M = M
  > module Unpacked = (val (module struct end : Ast.S) : Ast.S)
  > module Make () : sig type t val x : t end = struct
  >   type t = int
  >   let x = 1
  > end
  > module Fresh = Make ()
  > %}
  > %token EOF
  > %start <_> main
  > %%
  > main: EOF { (M.x, (module struct end : Ast.S)) }
  > EOF

  $ dune build lib.cma
  File "outer/inner/inner__mock.ml.mock", line 1:
  Error (warning 63 [erroneous-printed-signature]): The printed interface
    differs from the inferred interface. The inferred interface contained items
    which could not be printed properly due to name collisions between
    identifiers.
    File "_none_", line 1:
    Definition of module Lib__Outer__/2
  File "_none_", line 1:
    Definition of module Lib__Outer__Inner__/2
    Beware that this warning is purely informational and will not catch all
    instances of erroneous printed interface.
  [1]
